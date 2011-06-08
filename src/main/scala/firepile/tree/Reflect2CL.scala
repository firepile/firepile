package firepile.tree

import firepile.tree.Trees.{
  Tree => CLTree, 
  Assign => CLAssign, 
  Block => CLBLock, 
  Select => CLSelect, 
  Return => CLReturn, 
  ArrayDef => CLArrayDef,
  ReturnVoid => CLReturnVoid,
  Seq => TreeSeq,
  If => CLIf,
  _}
import scala.reflect._
import firepile.compiler.Return
import firepile.compiler.EmptyTree
import firepile.compiler.DummyTree
import firepile.compiler.ArrayDef
import firepile.compiler.Struct
import firepile.compiler.TypeDef
import firepile.compiler.FunctionDec

object Reflect2CL {
  implicit def toCLTree(t: scala.reflect.Tree): CLTree = t match {
    case Function(params, Block(body, Ident(Method(funName,retTyp)))) if firepile.compiler.JVM2Reflect.kernelMethodName.equals(funName) => {
      KernelFunDef(Id(funName), params.map(p => p match {
          case f@LocalValue(_, name, typ) => rewriteFormal(f) // Formal(translateType(typ), Id(name))
          case _ => throw new RuntimeException("Param not a LocalValue")
        }), body.map(b => toCLTree(b)):_*)
    }
    case Function(params, Block(body, Ident(Method(funName,retTyp)))) => {
      FunDef(translateType(retTyp),Id(funName), params.map(p => p match {
          case f@LocalValue(_, name, typ) => rewriteFormal(f) // Formal(translateType(typ), Id(name))
          case _ => throw new RuntimeException("Param not a LocalValue")
        }), body.map(b => toCLTree(b)):_*)
    }
    case FunctionDec(ret, name, params) => FunDec(translateType(ret), name, params.map(p => p match {
        case f: LocalValue => rewriteFormal(f)
        case _ => throw new RuntimeException("Param not a LocalValue")
      }))
    // case Assign(Ident(LocalValue(_,lhs,_)),Ident(LocalValue(_,rhs,_))) => CLAssign(Id(lhs), Id(rhs))
    // Assign(Ident(LocalValue(NoSymbol,this,NamedType(Unmatched:PlusFuns))),This(NoSymbol))
    case Assign(_, This(_)) => TreeSeq()
    case Assign(l, r) => CLAssign(l, r)
//    case Apply(Select(Ident(LocalValue(_,op1,_)),Method(methodName,_)),List(Ident(LocalValue(NoSymbol,op2,_)))) => Bin(Id(op1),methodToBinOp(methodName), Id(op2))
    case Apply(Select(op1,Method("scala.Array.update",_)),List(op2, upd)) => CLAssign(ArrayAccess(op1,op2),upd)
    case Apply(Select(op1,Method("scala.Array.apply",_)),List(op2)) => ArrayAccess(op1,op2)
    case Apply(Select(op1,Method("apply",_)),List(op2)) => ArrayAccess(op1,op2)
    case Apply(Select(op1,Method("firepile.util.BufferBackedArray$BBArray.apply",_)),List(op2)) => ArrayAccess(op1,op2)
    case Apply(Select(op1,Method("firepile.util.BufferBackedArray$BBArray.update",_)),List(op2,upd)) => CLAssign(ArrayAccess(op1,op2),upd)
    case Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Item")))),
      Method(methodName,MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(index)) => 
                CLSelect(ArrayAccess(Id(fieldName),index), Id(shortMethodName(methodName)))
    case Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Group")))),
      Method(methodName,MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(index)) => 
                CLSelect(ArrayAccess(Id(fieldName),index), Id(shortMethodName(methodName)))
    case Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("scala.collection")),Class("scala.collection.SeqLike")))),
                          Method("scala.collection.SeqLike.size",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0)))
                        => CLSelect(ArrayAccess(Id(fieldName), IntLit(0)), Id("size"))
    case Apply(Select(Ident(LocalValue(NoSymbol,_,PrefixedType(ThisType(Class("firepile")),Class("firepile.Group")))),
      Method("firepile.Group.barrier",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List()) => Eval(Call(Id("barrier"), Id("CLK_LOCAL_MEM_FENCE")))
    case Apply(Select(op1,Method(methodName,_)),List(op2)) if methodName.endsWith("update") => ArrayAccess(op1,op2)
    case Apply(Select(op1,Method(methodName,_)),List(op2)) => methodToBinOp(methodName) match {
      case Some(op) => Bin(op1, op, op2)
      case None => Call(Id(methodName), List(toCLTree(op2)))
    }
    case Apply(Select(op1,Method(methodName,_)),ops) => Call(Id(methodName), ops.map(o => toCLTree(o)))
    case ValDef(LocalValue(_,"l0",typ), _) => TreeSeq()
    case ValDef(LocalValue(_,name,typ), _) => VarDef(translateType(typ), Id(name))
    case ArrayDef(LocalValue(_,name,typ), Literal(dim)) => CLArrayDef(Id(name), translateType(typ), IntLit(dim.asInstanceOf[Int]))
    case If(cond, ifTrue, ifFalse) => CLIf(cond, ifTrue, Nop)
    case Return(EmptyTree()) => CLReturnVoid
    case Goto(lbl) => GoTo(lbl.name)
    case Select(base, itm) => CLSelect(base, itm.name)
    case Return(ret) => CLReturn(toCLTree(ret))
    case Ident(v) => v
    case Struct(name, fields) => StructDef(Id(name), fields.map(f => toCLTree(f)))
    case TypeDef(NamedType(old), NamedType(nw)) => Eval(Id("typedef struct " + old + " " + nw))

    case Target(lbl,_) => lbl
    case Literal(value) => value match {
      case l: Int => IntLit(l)
      case l: Float => FloatLit(l)
      case l: Double => DoubleLit(l)
      case l => throw new RuntimeException("Unmatched literal: " + l)
    }
    case EmptyTree() => TreeSeq()
    case x => Id(x.toString)
  }

  implicit def toCLTree(s: scala.reflect.Symbol): CLTree = s match {
    case LocalValue(_, name, typ) => Id(name)
    case LabelSymbol(target) => Label(target)
    case Class(name) => Id(name)
    case Field(name, typ) => Id(name)
    case _ => Id("unsupported")
  }

  def methodToBinOp(methodName: String): Option[String] = shortMethodName(methodName) match {
    // case "$tilde" => "~"
    case "$eq" => Some("==")
    case "$neq" => Some("!=") // added
    case "$less" => Some("<")
    case "$leq" => Some("<=") // added
    case "$greater" => Some(">")
    case "$geq" => Some(">=") // added
    // case "$bang" => "!"
    case "$percent" => Some("%")
    case "$up" => Some("^")
    case "$amp" => Some("&")
    case "$bar" => Some("|")
    case "$times" => Some("*")
    case "$div" => Some("/")
    case "$plus" => Some("+")
    case "$minus" => Some("-")
    case "$rshift" => Some(">>")
    case "$lshift" => Some("<<")
    case x => None // throw new RuntimeException("Unsupported op method: " + x)
  }

  def shortMethodName(methodName: String): String = methodName.substring(methodName.lastIndexOf('.')+1)

  private def rewriteFormal(f: LocalValue) = {
    f match {
      case LocalValue(_,n,_) if n.startsWith("g_") && n.endsWith("_data") => Formal(MemType("global",PtrType(translateType(f.tpe))),Id(n.substring(n.indexOf("g_")+2)))
      case LocalValue(_,n,_) if n.startsWith("l_") && n.endsWith("_data") => Formal(MemType("local",PtrType(translateType(f.tpe))),Id(n.substring(n.indexOf("l_")+2)))
      case LocalValue(_,n,_) if n.startsWith("p_") && n.endsWith("_data") => Formal(MemType("private",PtrType(translateType(f.tpe))),Id(n.substring(n.indexOf("p_")+2)))
      case LocalValue(_,n,_) if n.startsWith("c_") && n.endsWith("_data") => Formal(MemType("constant",PtrType(translateType(f.tpe))),Id(n.substring(n.indexOf("c_")+2)))
      case LocalValue(_,n,_) if n.startsWith("g_") && translateType(f.tpe).asInstanceOf[ValueType].name.equals("int") => Formal(ConstType(translateType(f.tpe)),Id(n.substring(n.indexOf("g_")+2)))   // for NVIDIA on Windows, works on other platforms too
      case LocalValue(_,n,_) if n.startsWith("g_") => Formal(MemType("global",translateType(f.tpe)),Id(n.substring(n.indexOf("g_")+2)))
      case LocalValue(_,n,_) if n.startsWith("l_") => Formal(MemType("local",translateType(f.tpe)),Id(n.substring(n.indexOf("l_")+2)))
      case LocalValue(_,n,_) if n.startsWith("p_") => Formal(MemType("private",translateType(f.tpe)),Id(n.substring(n.indexOf("p_")+2)))
      case LocalValue(_,n,_) if n.startsWith("c_") => Formal(MemType("constant",translateType(f.tpe)),Id(n.substring(n.indexOf("c_")+2)))
      case LocalValue(_,n,_) if n.startsWith("C_") => Formal(ConstType(translateType(f.tpe)),Id(n.substring(n.indexOf("C_")+2)))
      case LocalValue(_,n,NamedType(t)) if t.equals("firepile_Group") => Formal(PtrType(ValueType(t)),n)
      case LocalValue(_,n,NamedType(t)) if t.equals("firepile_Item") => Formal(PtrType(ValueType(t)),n)
      case LocalValue(_,n,NamedType(t)) if t.equals("kernel_ENV") => Formal(ValueType(t),n)
      case n => Formal(ConstType(translateType(f.tpe)), Id(f.name))
    }
  }

  implicit def translateType(t: Type): CLTree = t match {
    case PrefixedType(ThisType(Class("scala")),Class("scala.Any")) => ValueType("void")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Boolean")) => ValueType("BOOL")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Byte")) => ValueType("char")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Short")) => ValueType("short")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Char")) => ValueType("char")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Int")) => ValueType("int")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Long")) => ValueType("long")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Float")) => ValueType("float")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Double")) => ValueType("double")
    case NamedType("P_firepile_Item") => PtrType(ValueType("firepile_Item"))
    case NamedType("P_firepile_Group") => PtrType(ValueType("firepile_Group"))
    case NamedType(name) if !name.endsWith("Array") => name match {
      case n if n.startsWith("g_") => MemType("global",PtrType(ValueType(translateType(n.substring(n.indexOf("g_")+2)))))
      case n if n.startsWith("l_") => MemType("local",PtrType(ValueType(translateType(n.substring(n.indexOf("l_")+2)))))
      case n if n.startsWith("p_") => MemType("private",PtrType(ValueType(translateType(n.substring(n.indexOf("p_")+2)))))
      case n if n.startsWith("c_") => MemType("constant",PtrType(ValueType(translateType(n.substring(n.indexOf("c_")+2)))))
      case n => ValueType(translateType(n)) // ValueType(firepile.compiler.JVM2Reflect.mangleName(n)) // ValueType(translateType(n))
    }
    case NamedType(name) => ValueType(name)
    case x => ValueType("unmatched type: " + x) // throw new RuntimeException("unsupported type: " + x)  
  }

  def translateType(fullname: String) = firepile.compiler.JVM2Reflect.mangleName(fullname) match {
    case "scala_Float" => "float"
    case "scala_Int" => "int"
    case x => x
  }
}

