package firepile.tree

object Trees {
    trait Tree {
        def toCL: String
    }

    def stmt(t: Tree) = {
      t match {
        case s @ Id(_) => Eval(s).toCL
        case s @ Select(_,_) => Eval(s).toCL
        case s @ IntLit(_) => Eval(s).toCL
        case s @ LongLit(_) => Eval(s).toCL
        case s @ FloatLit(_) => Eval(s).toCL
        case s @ DoubleLit(_) => Eval(s).toCL
        case s @ Un(_,_) => Eval(s).toCL
        case s @ Bin(_,_,_) => Eval(s).toCL
        case s @ Call(_,_) => Eval(s).toCL
        case s @ Cast(_,_) => Eval(s).toCL
        case s @ Assign(_,_) => Eval(s).toCL
        case s => s.toCL
      }
    }

    def paren(t: Tree, s: Tree) = {
        def paren(t: Tree) = "(" + t.toCL + ")"

        def prio(t: Tree) = t match {
            case Id(_) => 1
            case IntLit(_) => 1
            case LongLit(_) => 1
            case FloatLit(_) => 1
            case DoubleLit(_) => 1
            case Select(_,_) => 2
            case Call(_,_) => 3
            case Un(_,_) => 4
            case Cast(_,_) => 5
            case Bin(_,"*",_) => 6
            case Bin(_,"/",_) => 6
            case Bin(_,"%",_) => 6
            case Bin(_,"+",_) => 7
            case Bin(_,"-",_) => 7
            case Bin(_,"<<",_) => 8
            case Bin(_,">>",_) => 8
            case Bin(_,">>>",_) => 8
            case Bin(_,"^",_) => 9
            case Bin(_,"|",_) => 9
            case Bin(_,"&",_) => 9
            case Bin(_,"<",_) => 9
            case Bin(_,">",_) => 9
            case Bin(_,"<=",_) => 9
            case Bin(_,">=",_) => 9
            case Bin(_,"==",_) => 10
            case Bin(_,"!=",_) => 10
            case Assign(_,_) => 20
            case _ => 99
        }

        t match {
          case Assign(_,x) if x == s => s.toCL
          case _ => {
            if (prio(s) >= prio(t))
                paren(s)
            else
                s.toCL
          }
        }
    }

    def escape(str: String): String = {
      var s = ""
      for (ch <- str) {
        s += escape(ch)
      }
      s
    }

    def padWithZero(str: String) = {
      var s = str
      while (s.length < 3)
        s = "0" + s
      s
    }

    def escape(ch: Char): String = ch match {
      case ch if ch == '\f' => "\\f"
      case ch if ch == '\t' => "\\t"
      case ch if ch == '\b' => "\\b"
      case ch if ch == '\r' => "\\r"
      case ch if ch == '\n' => "\\n"
      case ch if ch == '\"' => "\\\""
      case ch if ch == '\\' => "\\\\"
      case ch if ch == '\'' => "\\'"
      case ch if ch.isControl || ch > 127 => "\\" + padWithZero(ch.asDigit.toOctalString)
      case ch => ch.toString
    }

    case class Id(name: String) extends Tree {
        def toCL = name
    }
    case class IntLit(value: Int) extends Tree {
        def toCL = value.toString
    }
    case class LongLit(value: Long) extends Tree {
        def toCL = value.toString
    }
    case class FloatLit(value: Float) extends Tree {
        def toCL = value.toString + "f"
    }
    case class DoubleLit(value: Double) extends Tree {
        def toCL = value.toString
    }
    case class CharLit(value: Char) extends Tree {
        def toCL = "'" + escape(value.toString) + "'"
    }
    case class StringLit(value: String) extends Tree {
        def toCL = "\"" + escape(value.toString) + "\""
    }
    case class Cast(typ: Tree, e: Tree) extends Tree {
        def toCL = "(" + typ.toCL + ") " + paren(this, e)
    }
    case class Bin(e1: Tree, op: String, e2: Tree) extends Tree {
        def toCL = paren(this, e1) + " " + op + " " + paren(this, e2)
    }
    case class Un(op: String, e: Tree) extends Tree {
        def toCL = op + paren(this, e)
    }
    case class ArrayAccess(e1: Tree, e2: Tree) extends Tree {
        def toCL = paren(this, e1) + "[" + e2.toCL + "]"
    }
    case class Assign(e1: Tree, e2: Tree) extends Tree {
        def toCL = paren(this, e1) + " = " + paren(this, e2)
    }

    case class Ref(e: Tree) extends Tree {
        def toCL = "&" + paren(this, e)
    }
    case class Deref(e: Tree) extends Tree {
        def toCL = "*" + paren(this, e)
    }
    object Select {
        def apply(target: Tree, name: Id): Select = Select(target, name.name)
    }
    case class Select(target: Tree, name: String) extends Tree {
        def toCL = target match {
            case Deref(e) => paren(this, e) + "->" + name
            case _ => paren(this, target) + "." + name
        }
    }

    object Call {
        def apply(fun: Tree, args: Tree*): Call = Call(fun, args.toList)
    }
    case class Call(fun: Tree, args: List[Tree]) extends Tree {
        def toCL = paren(this, fun) + args.map((t:Tree) => t.toCL).mkString("(", ", ", ")")
    }
    case class If(cond: Tree, e1: Tree, e2: Tree) extends Tree {
        def toCL = (e1,e2) match {
          case (GoTo(_),Nop) => "if (" + cond.toCL + ") " + stmt(e1)
          case (e1,Nop) => "if (" + cond.toCL + ") {\n" + indent(stmt(e1)) + "}\n"
          case (e1,e2) => "if (" + cond.toCL + ") {\n" + indent(stmt(e1)) + "} else {\n" + indent(stmt(e2)) + "}\n"
        }
    }
    case class Eval(e: Tree) extends Tree {
        def toCL = e.toCL + ";\n"
    }
    case class Return(e: Tree) extends Tree {
        def toCL = "return " + e.toCL + ";\n"
    }
    case object Return extends Tree {
        def toCL = "return;\n"
    }
    case object Nop extends Tree {
        def toCL = ";\n"
    }
    case class While(cond: Tree, body: Tree) extends Tree {
        def toCL = "while (" + cond.toCL + ") {\n" + indent(stmt(body)) + "}"
    }
    case class DoWhile(body: Tree, cond: Tree) extends Tree {
        def toCL = "do {\n" + indent(stmt(body)) + "} while (" + cond.toCL + ");\n"
    }
    case class Switch(index: Tree, cases: List[Tree]) extends Tree {
        def toCL = "switch (" + index.toCL + ") {\n" +
            cases.map((t:Tree) => indent(stmt(t))).mkString("") + "}\n"
    }
    case class Case(index: Tree, body: Tree) extends Tree {
        def toCL = "case " + index.toCL + ": {\n" + indent(stmt(body)) + "}\n"
    }
    case class Default(body: Tree) extends Tree {
        def toCL = "default: {\n" + indent(stmt(body)) + "}\n"
    }
    case class Block(decls: List[Tree], body: Tree) extends Tree {
      def toCL = "{\n" + decls.map((t:Tree) => indent(stmt(t))).mkString("") +
                indent(body.toCL) + "}"
    }
    object Comma {
      def apply(exps: Tree*): Comma = Comma(exps.toList)
    }
    case class Comma(exps: List[Tree]) extends Tree {
      def toCL = exps.map((t:Tree) => t.toCL).mkString("(", ", ", ")")
    }
    object Seq {
      def apply(stmts: Tree*): Seq = Seq(stmts.toList)
    }
    case class Seq(stmts: List[Tree]) extends Tree {
      def toCL = stmts.map((t:Tree) => stmt(t)).mkString("")
    }
    object VarDef {
        def apply(typ: Tree, name: Id): VarDef = VarDef(typ, name.name)
    }
    case class VarDef(typ: Tree, name: String) extends Tree {
        def toCL = typ.toCL + " " + name + ";\n"
    }
    object Formal {
        def apply(typ: Tree, name: Id): Formal = Formal(typ, name.name)
    }
    case class Formal(typ: Tree, name: String) extends Tree {
        def toCL = typ.toCL + " " + name
    }
    object FunDef {
        def apply(typ: Tree, name: Id, formals: List[Tree], body: Tree): FunDef = FunDef(typ, name.name, formals, body)
        def apply(typ: Tree, name: Id, formals: List[Tree], body: Tree*): FunDef = FunDef(typ, name.name, formals, Seq(body:_*))
    }
    case class FunDef(typ: Tree, name: String, formals: List[Tree], body: Tree) extends Tree {
        def toCL = typ.toCL + " " + name + formals.map((t:Tree) => t.toCL).mkString("(", ", ", ") {\n") + indent(body.toCL) + "}\n\n"

    }
    object KernelFunDef {
        def apply(name: Id, formals: List[Tree], body: Tree): KernelFunDef = KernelFunDef(name.name, formals, body)
        def apply(name: Id, formals: List[Tree], body: Tree*): KernelFunDef = KernelFunDef(name.name, formals, Seq(body:_*))
    }
    case class KernelFunDef(name: String, formals: List[Tree], body: Tree) extends Tree {
        def toCL = "__kernel void " + name + formals.map((t:Tree) => t.toCL).mkString("(", ", ", ") {\n") + indent(body.toCL) + "}\n\n"
    }
    object StructDef {
        def apply(name: Id, fields: List[Tree]): StructDef = StructDef(name.name, fields)
        def apply(name: Id, fields: Tree*): StructDef = StructDef(name.name, fields.toList)
        def apply(name: String, fields: Tree*): StructDef = StructDef(name, fields.toList)
    }
    case class StructDef(name: String, fields: List[Tree]) extends Tree {
        def toCL = "struct " + name + " {\n" +
                         fields.map((t:Tree) => indent(t.toCL)).mkString("") +
                        "};\n\n"
    }
    case class ValueType(name: String) extends Tree {
        def toCL = name
    }
    case class MemType(name: String, typ: Tree) extends Tree {
        def toCL = "__" + name + " " + typ.toCL
    }
    case class ConstType(typ: Tree) extends Tree {
        def toCL = "const " + typ.toCL
    }
    object StructLit {
        def apply(fields: Tree*): StructLit = StructLit(fields.toList)
    }
    case class StructLit(fields: List[Tree]) extends Tree {
        def toCL = "{ " + fields.map((t:Tree) => t.toCL).mkString(", ") + " }"
    }
    object StructType {
        def apply(name: Id): StructType = StructType(name.name)
    }
    case class StructType(name: String) extends Tree {
        def toCL = "struct " + name
    }
    object ArrayDef {
        def apply(name: Id, typ: Tree, size: Tree): ArrayDef = ArrayDef(name.name, typ, size)
    }
    case class ArrayDef(name: String, typ: Tree, size: Tree) extends Tree {
        def toCL = typ.toCL + " " + name + "[" + size.toCL + "];\n"
    }
    case class PtrType(typ: Tree) extends Tree {
        def toCL = typ.toCL + "*"
    }
    case class Label(name: String) extends Tree {
        def toCL = name + ":\n"
    }
    case class GoTo(target: String) extends Tree {
        def toCL = "goto " + target + ";\n"
    }
    case class GetLocalId(typ: Tree) extends Tree {
        def toCL = "get_local_id(" + typ.toCL + ")"
    }
    case class GetGlobalId(typ: Tree) extends Tree {
        def toCL = "get_global_id(" + typ.toCL + ")"
    }


  def forallTree(f: Tree => Unit) = fold((t:Tree) => {f(t); t}) _

  def fold(f: Tree => Tree)(t: Tree): Tree = f(t match {
    case Call(fun, args) => Call(fold(f)(fun), args.map(a => fold(f)(a)))
    case Switch(e, cases) => Switch(fold(f)(e), cases.map(a => fold(f)(a)))
    case Case(e, s) => Case(fold(f)(e), fold(f)(s))
    case Bin(op1, op, op2) => Bin(fold(f)(op1), op, fold(f)(op2))
    case Un(op, op2) => Un(op, fold(f)(op2))
    case Eval(e) => Eval(fold(f)(e))
    case Assign(op1, op2) => Assign(fold(f)(op1), fold(f)(op2))
    case Select(op1, name) => Select(fold(f)(op1), name)
    case Ref(op1) => Ref(fold(f)(op1))
    case Deref(op1) => Deref(fold(f)(op1))
    case ArrayAccess(op1, op2) => ArrayAccess(fold(f)(op1), fold(f)(op2))
    case Cast(t, e) => Cast(t, fold(f)(e))
    case If(e, s1, s2) => If(fold(f)(e), fold(f)(s1), fold(f)(s2))
    case While(e, s) => While(fold(f)(e), fold(f)(s))
    case DoWhile(s, e) => DoWhile(fold(f)(s), fold(f)(e))
    case Return(e) => Return(fold(f)(e))
    case t => t
  })

    val IntType = ValueType("int")
    val FloatType = ValueType("float")
    val LongType = ValueType("long")
    val DoubleType = ValueType("double")
    val CharType = ValueType("char")
    

    implicit def int2IntLit(n: Int) = IntLit(n)
    implicit def float2FloatLit(n: Float) = FloatLit(n)
    implicit def long2LongLit(n: Long) = LongLit(n)
    implicit def double2DoubleLit(n: Double) = DoubleLit(n)
    implicit def wrapTree(t: Tree) = new WrappedTree(t)

    class WrappedTree(t1: Tree) {
        def +(t2: Tree) = Bin(t1, "+", t2)
        def -(t2: Tree) = Bin(t1, "-", t2)
        def *(t2: Tree) = Bin(t1, "*", t2)
        def /(t2: Tree) = Bin(t1, "/", t2)
        def %(t2: Tree) = Bin(t1, "%", t2)
        def <<(t2: Tree) = Bin(t1, "<<", t2)
        def >>(t2: Tree) = Bin(t1, ">>", t2)
        def <(t2: Tree) = Bin(t1, "<", t2)
        def >(t2: Tree) = Bin(t1, ">", t2)
        def <=(t2: Tree) = Bin(t1, "<=", t2)
        def >=(t2: Tree) = Bin(t1, ">=", t2)
        def ==(t2: Tree) = Bin(t1, "==", t2)
        def !=(t2: Tree) = Bin(t1, "!=", t2)
        def :=(t2: Tree) = Assign(t1, t2)
        def then(t2: Tree) = Seq(t1, t2)
    }

    def indent(s: String) = {
        var t = ""
        val tab = "  " 
        for (si <- s.linesWithSeparators) {
            t += tab + si
        }
        t
    }
}
