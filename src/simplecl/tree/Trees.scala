package simplecl.tree

object Trees {
    trait Tree {
        def toCL: String
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

        if (prio(s) >= prio(t))
            paren(s)
        else
            s.toCL
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

    case class Select(target: Tree, name: String) extends Tree {
        def toCL = paren(this, target) + "." + name
    }

    case class Call(fun: Tree, args: List[Tree]) extends Tree {
        def toCL = paren(this, fun) + args.map((t:Tree) => t.toCL).mkString("(", ", ", ")")
    }
    case class If(cond: Tree, e1: Tree, e2: Tree) extends Tree {
        def toCL = "if (" + cond.toCL + ") {\n" + indent(e1.toCL) + "} else {\n" + indent(e2.toCL) + "}\n"
    }
    case class Eval(e: Tree) extends Tree {
        def toCL = e.toCL + ";\n"
    }
    case class Return(e: Tree) extends Tree {
        def toCL = "return " + e.toCL + ";\n"
    }
    case class ReturnV extends Tree {
        def toCL = "return;\n"
    }
    case class While(cond: Tree, body: Tree) extends Tree {
        def toCL = "while (" + cond.toCL + ") {\n" + indent(body.toCL) + "}"
    }
    case class DoWhile(body: Tree, cond: Tree) extends Tree {
        def toCL = "do {\n" + indent(body.toCL) + "} while (" + cond.toCL + ");\n"
    }
    case class Switch(index: Tree, cases: List[Tree]) extends Tree {
        def toCL = "switch (" + index.toCL + ") {\n" +
            cases.map((t:Tree) => indent(t.toCL)).mkString(indent("break;\n")) + "}"
    }
    case class Case(index: Tree, body: Tree) extends Tree {
        def toCL = "case " + index.toCL + ": {\n" + indent(body.toCL) + "}\n"
    }
    case class Block(decls: List[Tree], body: Tree) extends Tree {
      def toCL = "{\n" + decls.map((t:Tree) => indent(t.toCL)).mkString("") +
                indent(body.toCL) + "}"
    }
    case class Seq(stmts: List[Tree]) extends Tree {
      def toCL = stmts.map((t:Tree) => indent(t.toCL)).mkString("")
    }
    case class VarDef(typ: Tree, name: String) extends Tree {
        def toCL = typ.toCL + " " + name + ";\n"
    }
    case class Formal(typ: Tree, name: String) extends Tree {
        def toCL = typ.toCL + " " + name
    }
    case class FunDef(typ: Tree, name: String, formals: List[Tree], body: Tree) extends Tree {
        def toCL = typ.toCL + " " + name + formals.map((t:Tree) => t.toCL).mkString("(", ", ", ") {\n") + indent(body.toCL) + "}\n\n"

    }
    case class StructDef(name: String, fields: List[Tree]) extends Tree {
        def toCL = "struct " + name + " {\n" +
                         fields.map((t:Tree) => indent(t.toCL)).mkString("") +
                        "};\n\n"
    }

    def main(args: Array[String]) = {
        val t = FunDef(Id("int"), "fact", List[Tree](Formal(Id("int"), "n")),
                       Seq(List[Tree](If(Bin(Id("n"), "<=", IntLit(1)),
                                Return(IntLit(1)),
                                Return(Bin(Id("n"), "*", Call(Id("fact"), List[Tree](Bin(Id("n"), "-", IntLit(1))))))))))
        println(t.toCL)
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
