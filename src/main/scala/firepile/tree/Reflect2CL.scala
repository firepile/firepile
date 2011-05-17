package firepile.tree

import firepile.tree.Trees.{Tree => CLTree, Assign => CLAssign, Block => CLBLock, Select => CLSelect, Return => CLReturn, _}
import scala.reflect._
import firepile.compiler.Return

object Reflect2CL {
  def toCLTree(t: scala.reflect.Tree): CLTree = t match {
    case Function(params, Block(body, Ident(Method(funName,retTyp)))) => {
      FunDef(translateType(retTyp),Id(funName), params.map(p => p match {
          case LocalValue(_, name, typ) => Formal(translateType(typ), Id(name))
          case _ => throw new RuntimeException("Param not a LocalValue")
        }), body.map(b => toCLTree(b)):_*)
    }
    case Assign(Ident(LocalValue(_,lhs,_)),Ident(LocalValue(_,rhs,_))) => CLAssign(Id(lhs), Id(rhs))
    case Apply(Select(Ident(LocalValue(_,op1,_)),Method(methodName,_)),List(Ident(LocalValue(NoSymbol,op2,_)))) => Bin(Id(op1),methodToBinOp(methodName), Id(op2))
    case Return(ret) => CLReturn(toCLTree(ret))
    case x => Id(x.toString)
  }

  def toCLTree(s: scala.reflect.Symbol) = s match {
    case LocalValue(_, name, typ) => Id(name)
    case _ => Id("unsupported")
  }

  def methodToBinOp(methodName: String) = methodName.substring(methodName.lastIndexOf('.')+1) match {
    // case "$tilde" => "~"
    case "$eq" => "="
    case "$less" => "<"
    case "$greater" => ">"
    // case "$bang" => "!"
    case "$percent" => "%"
    case "$up" => "^"
    case "$amp" => "&"
    case "$bar" => "|"
    case "$times" => "*"
    case "$div" => "/"
    case "$plus" => "+"
    case "$minus" => "-"
    case _ => throw new RuntimeException("Unsupported op method")
  }

  def translateType(t: Type): ValueType = t match {
    case PrefixedType(ThisType(Class("scala")),Class("scala.Any")) => ValueType("void")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Boolean")) => ValueType("BOOL")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Byte")) => ValueType("char")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Short")) => ValueType("short")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Char")) => ValueType("char")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Int")) => ValueType("int")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Long")) => ValueType("long")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Float")) => ValueType("float")
    case PrefixedType(ThisType(Class("scala")),Class("scala.Double")) => ValueType("double")
    case _ => throw new RuntimeException("unsupported type")  
  }
}

