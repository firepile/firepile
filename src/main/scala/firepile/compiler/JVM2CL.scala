package firepile.compiler

import scala.tools.scalap._
import scala.tools.scalap.{Main => Scalap}
import scala.tools.scalap.scalax.rules.scalasig._

import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import soot.jimple.toolkits.callgraph.CallGraphBuilder
import soot.jimple.toolkits.invoke.StaticInliner
import soot.jimple.toolkits.invoke.StaticMethodBinder
import soot.options.Options

import soot.Body
import soot.{Unit => SootUnit}
import soot.Scene
import soot.Value
import soot.ValueBox
import soot.Local
import soot.SootClass
import soot.SootMethod
import soot.SootMethodRef
import soot.Modifier
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.toolkits.graph.UnitGraph
import soot.jimple.JimpleBody
import soot.jimple.Jimple
import soot.grimp.Grimp
import soot.grimp.GrimpBody
import soot.{Type=>SootType}
import soot.jimple.Stmt
import soot.{VoidType => SootVoidType}
import soot.{BooleanType => SootBooleanType}
import soot.{ByteType => SootByteType}
import soot.{ShortType => SootShortType}
import soot.{CharType => SootCharType}
import soot.{IntType => SootIntType}
import soot.{LongType => SootLongType}
import soot.{FloatType => SootFloatType}
import soot.{DoubleType => SootDoubleType}
import soot.{RefType => SootRefType}
import soot.{ArrayType => SootArrayType}
import soot.{NullType => SootNullType}

import firepile.compiler.util.ScalaTypeGen
import firepile.tree.Trees._
import firepile.tree.Trees.{Seq=>TreeSeq}
import scala.Seq
import soot.jimple.{ FloatConstant,
                     DoubleConstant,
                     IntConstant,
                     LongConstant,
                     StringConstant }
import firepile.compiler.GrimpUnapply._

import scala.collection.mutable.HashMap

object JVM2CL {
  def main(args: Array[String]) = {
    if (args.length != 2) {
      println("usage: firepile.compile.JVM2CL className methodSig")
      exit(1)
    }

    val className = args(0)
    val methodSig = args(1)

    compileRoot(className, methodSig)
  }

  private val makeCallGraph = true
  private val HACK = false

  setup

  def compileRoot(className: String, methodSig: String, self: AnyRef = null): List[Tree] = {
    println("compiling " + className + "." + methodSig)
    try {
      //if (HACK) addRootMethodToWorklist("scala.collection.immutable.List", "dropWhile(Lscala/Function1;)Ljava/lang/Object;", null) else
      addRootMethodToWorklist(className, methodSig, self)
      if (makeCallGraph) {
        buildCallGraph
        optimizeCallGraph
      }
      processWorklist
    } catch {
      case e: ClassNotFoundException => {
        println("Class not found: " + e.getMessage)
        Nil
      }
    }
  }

  private def setup = {
    // java.class.path is broken in Scala, especially when running under sbt
    //Scene.v.setSootClassPath(Scene.v.defaultClassPath

    Scene.v.setSootClassPath(Scene.v.defaultClassPath
                  + ";."+";C:/Soot/lib/firepile.jar"
                  + ";C:/Soot/lib/firepilesoot.jar"
                  + ";C:/Soot/lib/firepilesoottest.jar"
                  + ";C:/Soot/lib/soot-2.4.0.jar"
                  + ";C:/Soot/lib/scalap.jar"
                  + ";C:/Soot/lib/rt.jar"
      + ";C:/ScalaOpencl2/firepile/lib/scala-library.jar")
    
    // might be useful if you want to relate back to source code
    Options.v.set_keep_line_number(true)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.setPhaseOption("cg", "safe-forname:false")
    // you can set context-sensitivity of call graph with this (more time consuming)
    // Options.v.setPhaseOption("cg", "context:1cfa") 
    Options.v.setPhaseOption("cg", "verbose:true")

    Options.v.set_allow_phantom_refs(true)
    if (makeCallGraph)
      Options.v.set_whole_program(true)
  }

  def methodName(m: SootMethod): String = mangleName(m.getDeclaringClass.getName + m.getName)
  def methodName(m: java.lang.reflect.Method): String = mangleName(m.getDeclaringClass.getName + m.getName)
  def methodName(m: SootMethodRef): String = mangleName(m.declaringClass.getName + m.name)
  def mangleName(name: String) = name.replace('$', '_').replace('.', '_')

  private implicit def v2tree(v: Value): Tree = translateExp(v)

  var next = 0
  def freshName(base: String = "tmp") = {
    next += 1
    base + next
  }

  class Worklist[A] extends Queue[A] {
    val inWorklist = new HashSet[A]

    override def +=(a: A) = {
      if (! inWorklist.contains(a)) {
        inWorklist += a
        super.+=(a)
      }
      else
        this
    }
  }

  trait Task {
    def run: List[Tree]
    def method: SootMethodRef
  }

  def mapDef(m: SootMethod, t: Tree) = t
  def reduceDef(m: SootMethod, t: Tree) = t

  object CompileMethodTask {
    def apply(m: SootMethod, self: AnyRef = null): CompileMethodTask = CompileMethodTask(m.makeRef, self)
    def apply(m: SootMethodRef): CompileMethodTask = CompileMethodTask(m, null)
  }
  
  case class CompileMethodTask(method: SootMethodRef, self: AnyRef) extends Task {
    def run = {
      val m = method

      // Force the class's method bodies to be loaded.
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.HIERARCHY)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.SIGNATURES)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.BODIES)
        
      compileMethod(m.resolve, self) match {
        case null => Nil
        case t => t::Nil
      }
    }
  }

  private val worklist = new Worklist[Task]

  private def addRootMethodToWorklist(className: String, methodSig: String, self: AnyRef): Unit = {
    // Set up the class we're working with
    val c = Scene.v.loadClassAndSupport(className)
    if (makeCallGraph) {
      Scene.v.loadNecessaryClasses
      c.setApplicationClass
    }

    // Retrieve the method and its body
    val i = c.methodIterator
    while (i.hasNext) {
      val m = i.next

      val sig = m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef)
      println("trying " + sig)
      if (sig.equals(methodSig)) {
        worklist += CompileMethodTask(m, self)
      }
    }
  }

  private def findSelf(v: Value, self: AnyRef): AnyRef = null

  private def buildCallGraph = {
    Scene.v.setEntryPoints(worklist.toList.map(p => p.method.resolve))
    println("entry points " + Scene.v.getEntryPoints)
    (new CallGraphBuilder).build
    println(Scene.v.getCallGraph)
  }

  private def optimizeCallGraph = {
    // Bind methods statically.
    StaticMethodBinder.v.transform
    StaticInliner.v.transform
  }

  private def processWorklist = {
    val results = ListBuffer[Tree]()

    while (! worklist.isEmpty) {
      val task = worklist.dequeue
      val ts = task.run
      results ++= ts
    }
    results.toList
  }

  def isStatic(flags: Int) = (flags & 0x0008) != 0
  def flagsToStr(clazz: Boolean, flags: Int): String = {
    val buffer = new StringBuffer()
    var x: StringBuffer = buffer
    if (((flags & 0x0007) == 0) &&
      ((flags & 0x0002) != 0))
      x = buffer.append("private ")
    if ((flags & 0x0004) != 0)
      x = buffer.append("protected ")
    if ((flags & 0x0010) != 0)
      x = buffer.append("final ")
    if ((flags & 0x0400) != 0)
      x = if (clazz) buffer.append("abstract ")
          else buffer.append("/*deferred*/ ")
    buffer.toString()
  }

  private def compileMethod(m: SootMethod, self: AnyRef): Tree = {
    println("-------------------------------------------------------")
    println(m)

    if (m.isAbstract)
        return null
    if (m.isNative)
        return null
    
    ScalaTypeGen.getScalaSignature(m.getDeclaringClass.getName.replaceAll("\\$",""))
 

    symtab = new SymbolTable(self)

    val b = m.retrieveActiveBody
    val gb = Grimp.v.newBody(b, "gb")

    val unitBuffer = ListBuffer[SootUnit]()
      for (u <- gb.getUnits) {
      unitBuffer += u
    }

    val units = unitBuffer.toList
    println("Grimp method body:")
    println(units.mkString("\n"))

    val body = translateUnits(units, Nil)
    val labeled = insertLabels(units, body, Nil)
    val fun = makeFunction(m, removeThis(labeled))

      // TODO: don't removeThis for normal methods; do removeThis for closures
      // TODO: verify that this is not used in the body of the method

    println()
    println("Result tree:")
    println(fun)

    println()
    println("Result CL:")
    println(fun.toCL)

    fun
  }

  private def removeThis(body: List[Tree]): List[Tree] = {
    body match {
      case Eval(Assign(v: Id, Id("_this")))::ts => {
        symtab.locals.remove(v)
        removeThis(ts)
      }
      case t::ts => t::removeThis(ts)
      case Nil => Nil
    }
  }

  val ANY_TYPE = ValueType("__any__")

  private class SymbolTable(val self: AnyRef) {
    val labels = new HashMap[SootUnit, String]()
    val params = new HashMap[Int, (Id, Tree)]()
    val locals = new HashMap[Id, Tree]()
    val arrays = new HashMap[Id, (Tree /*type*/, Tree /*size*/)]()
    var thisParam: (Id, Tree) = null

    def addThisParam(typ: SootType, id: Id) = {
      thisParam = (id, translateType(typ))
    }

    def addParamVar(typ: SootType, index: Int, id: Id) = {
      params += index -> (id, translateType(typ))
    }

    def addLocalVar(typ: SootType, id: Id) = {
      assert(!params.contains(id))
      // assert(!locals.contains(id))
      locals += id -> translateType(typ)
    }

    def addLocalVar(typ: Tree, id: Id) = {
      assert(!params.contains(id))
      // assert(!locals.contains(id))
      locals += id -> typ
    }

    def addArrayDef(typ: SootType, id: Id, size: Tree) = {
      assert(!locals.contains(id))
      arrays += id -> (translateType(typ), size)
    }

    var next = -1

    def nextLabel = {
      next += 1
      "lbl_" + next
    }
  }

  private var symtab: SymbolTable = null

  private def translateLabel(u: SootUnit): String = u match {
    case target : Stmt => {
      symtab.labels.get(target) match {
        case Some(label) => label
        case None => {
          val label = symtab.nextLabel
          symtab.labels += target -> label
          label
        }
      }
    }
    case _ => "Label"
  }

  private def translateType(t: SootType): Tree = t match {
      case t : SootVoidType => ValueType("void")
      case t : SootBooleanType => ValueType("int")
      case t : SootByteType => ValueType("char")
      case t : SootShortType => ValueType("short")
      case t : SootCharType => ValueType("ushort")
      case t : SootIntType => ValueType("int")
      case t : SootLongType => ValueType("long")
      case t : SootFloatType => ValueType("float")
      case t : SootDoubleType => ValueType("double")
      case t : SootRefType => {
        t.getSootClass.getName match {
          case "scala.Tuple2" => StructType("Tuple2")
          case "scala.Tuple3" => StructType("Tuple3")
          case "scala.Tuple4" => StructType("Tuple4")
          case "scala.Tuple5" => StructType("Tuple5")
          case "scala.Tuple6" => StructType("Tuple6")
          case _ => PtrType(ValueType(mangleName(t.toString)))
        }
      }
      case t : SootArrayType => PtrType(translateType(t.getArrayElementType))
      case t : SootNullType => PtrType(ValueType("void"))
      // TODO: array types
      case _ => ValueType(t.toString)
  }

  object ScalaMathCall {
      def unapply(v: Value): Option[(String,List[Value])] = {
        v match {
        // scala.Math.sin(x)  [deprecated]
        case GVirtualInvoke(GStaticFieldRef(SFieldRef(SClassName("scala.Math$"), "MODULE$", _, _)), SMethodRef(SClassName("scala.MathCommon"), name, _, _, _), args) => Some((name, args))
        case GVirtualInvoke(GStaticFieldRef(SFieldRef(SClassName("scala.Math$"), "MODULE$", _, _)), SMethodRef(SClassName("scala.Math$"), name, _, _, _), args) => Some((name, args))
        // scala.math.package$.sin(x)
        case GVirtualInvoke(GStaticFieldRef(SFieldRef(SClassName("scala.math.package$"), "MODULE$", _, _)), SMethodRef(SClassName("scala.MathCommon"), name, _, _, _), args) => Some((name, args))
        case GVirtualInvoke(GStaticFieldRef(SFieldRef(SClassName("scala.math.package$"), "MODULE$", _, _)), SMethodRef(SClassName("scala.math.package$"), name, _, _, _), args) => Some((name, args))
        // java.lang.Math.sin(x)
        case GStaticInvoke(SMethodRef(SClassName("java.lang.Math"), name, _, _, _), args) => Some((name, args))
        case _ => None
      }
      }
  }

  object FirepileMathCall {
      def unapply(v: Value): Option[(String,List[Value])] = {
        v match {
        // firepile.util.Math.sin(x)
        case GVirtualInvoke(GStaticFieldRef(SFieldRef(SClassName("firepile.util.Math$"), "MODULE$", _, _)), SMethodRef(SClassName("firepile.util.Math$"), name, _, _, _), args) => Some((name, args))
        case _ => None
      }
      }
  }

  object FloatMathCall {
    def unapply(v: Value): Option[(String,List[Value])] = v match {
      // (float) sin((double) x) --> sin(x)
      case GCast(DoubleMathCall(name, args), f) if f.equals(SootFloatType.v) => Some((name, args))
      case _ => None
    }
  }

  object DoubleMathCall {
    def unapply(v: Value): Option[(String,List[Value])] =
      v match {
      // sin((double) x) --> (double) sin(x)
      case ScalaMathCall(name, List(GCast(FloatTyped(x), d))) if d.equals(SootDoubleType.v) => Some((name, List(x)))
      case _ => None
    }
  }

  object FloatMath {
    def unapply(v: Value) = {
      val t: Tree = v match {
        case FloatMathCall("sin", List(x)) => Call(Id("sin"), x)
        case FloatMathCall("cos", List(x)) => Call(Id("cos"), x)
        case FloatMathCall("tan", List(x)) => Call(Id("tan"), x)
        case FloatMathCall("asin", List(x)) => Call(Id("asin"), x)
        case FloatMathCall("acos", List(x)) => Call(Id("acos"), x)
        case FloatMathCall("atan", List(x)) => Call(Id("atan"), x)
        case _ => null
      }
      if (t == null) None else Some(t)
    }
  }

  object DoubleMath {
    def unapply(v: Value) = {
      val t: Tree = v match {
        case DoubleMathCall("sin", List(x)) => Cast(ValueType("double"), Call(Id("sin"), x))
        case DoubleMathCall("cos", List(x)) => Cast(ValueType("double"), Call(Id("cos"), x))
        case DoubleMathCall("tan", List(x)) => Cast(ValueType("double"), Call(Id("tan"), x))
        case DoubleMathCall("asin", List(x)) => Cast(ValueType("double"), Call(Id("asin"), x))
        case DoubleMathCall("acos", List(x)) => Cast(ValueType("double"), Call(Id("acos"), x))
        case DoubleMathCall("atan", List(x)) => Cast(ValueType("double"), Call(Id("atan"), x))
        case _ => null
      }
      if (t == null) None else Some(t)
    }
  }

  object TupleSelect {
    def unapply(v: Value) = v match {
        // x._1() --> x._1
        case GVirtualInvoke(base, SMethodRef(k @ SClassName("scala.Tuple2"), "_1", _, _, _), Nil) => Some(Select(base, "_1"))
        case GVirtualInvoke(base, SMethodRef(k @ SClassName("scala.Tuple2"), "_2", _, _, _), Nil) => Some(Select(base, "_2"))
        case GVirtualInvoke(base, SMethodRef(k @ SClassName("scala.Tuple3"), "_1", _, _, _), Nil) => Some(Select(base, "_1"))
        case GVirtualInvoke(base, SMethodRef(k @ SClassName("scala.Tuple3"), "_2", _, _, _), Nil) => Some(Select(base, "_2"))
        case GVirtualInvoke(base, SMethodRef(k @ SClassName("scala.Tuple3"), "_3", _, _, _), Nil) => Some(Select(base, "_3"))
        case _ => None
    }
  }

  object UnboxCall {
    def unapply(v: Value) = v match {
        // scala.runtime.BoxesRunTime.unboxToFloat(Object) : float
        case GStaticInvoke(SMethodRef(k @ SClassName("scala.runtime.BoxesRunTime"), "unboxToInt", _, _, _), List(value)) =>
          Some(Select(Cast(ANY_TYPE, translateExp(value)), "i"))
        case GStaticInvoke(SMethodRef(k @ SClassName("scala.runtime.BoxesRunTime"), "unboxToFloat", _, _, _), List(value)) =>
          Some(Select(Cast(ANY_TYPE, translateExp(value)), "f"))
        case _ => None
    }
  }

  // Split library calls into separate objects.
  // This avoids an OutOfMemory error in scalac.
  object LibraryCall {
    def unapply(v: Value) = {
      val t: Tree = v match {
        // These are just here as examples for matching Array.ofDim.  We need a realy strategy for translating newarray.
        case GVirtualInvoke(GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)), SMethodRef(_, "ofDim", _, _, _),
                List(size, GVirtualInvoke(GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)), SMethodRef(_, "Int", _, _, _), Nil))) =>
                  Call(Id("newIntArray"), List[Tree](size))

        case GVirtualInvoke(GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)), SMethodRef(_, "ofDim", _, _, _),
                List(size, GVirtualInvoke(GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)), SMethodRef(_, "Float", _, _, _), Nil))) =>
                  Call(Id("newFloatArray"), List[Tree](size))


        case UnboxCall(t) => t
        case TupleSelect(t) => t

        // firepile.util.Math.sin(x)
        case FirepileMathCall(name, args) => Call(Id(name), args.map(a => translateExp(a)))

        // Predef$.MODULE$.floatWrapper(x).abs()
        case GVirtualInvoke(
                GVirtualInvoke(
                  GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)),
                  SMethodRef(_, "floatWrapper", _, _, _),
                  List(value)),
                SMethodRef(_, "abs", _, _, _),
                Nil) =>
                  Call(Id("fabs"), List[Tree](value))

        // (float) scala.math.sin((double) x)
        case FloatMath(t) => t

        // scala.math.sin((double) x)
        case DoubleMath(t) => t

        // scala.math.sin(x)
        case ScalaMathCall(name, args) => Call(Id(name), args.map(a => translateExp(a)))

        case _ => null
      }
      if (t == null) None else Some(t)
    }
  }

  object IntTyped {
    def unapply(v: AnyRef) = v match {
      case x : Value if SootIntType.v.equals(x.getType) => Some(x)
      case x : ValueBox if SootIntType.v.equals(x.getValue.getType) => Some(x.getValue)
      case _ => None
    }
  }

  object LongTyped {
    def unapply(v: AnyRef) = v match {
      case x : Value if SootLongType.v.equals(x.getType) => Some(x)
      case x : ValueBox if SootLongType.v.equals(x.getValue.getType) => Some(x.getValue)
      case _ => None
    }
  }

  object DoubleTyped {
    def unapply(v: AnyRef) = v match {
      case x : Value if SootDoubleType.v.equals(x.getType) => Some(x)
      case x : ValueBox if SootDoubleType.v.equals(x.getValue.getType) => Some(x.getValue)
      case _ => None
    }
  }

  object FloatTyped {
    def unapply(v: AnyRef) = v match {
      case x : Value if SootFloatType.v.equals(x.getType) => Some(x)
      case x : ValueBox if SootFloatType.v.equals(x.getValue.getType) => Some(x.getValue)
      case _ => None
    }
  }

  private def isFunctionClass(k: SootClass): Boolean = {
    if (k.getName.equals("scala.runtime.AbstractFunction1"))
      true
    else if (k.getName.equals("scala.runtime.AbstractFunction2"))
      true
    else if (k.getName.equals("scala.runtime.AbstractFunction3"))
      true
    else if (k.hasSuperclass && isFunctionClass(k.getSuperclass))
      true
    else
      false
  }

  private def isFunction(t: SootType): Boolean = t match {
    case t : SootRefType => t.hasSootClass && isFunctionClass(t.getSootClass)
    case _ => false
  }

  private def translateExp(v: Value): Tree = v match {
    // Must be first
    case LibraryCall(t) => t

    case GNullConstant() => Id("NULL")

    case GIntConstant(value) => IntLit(value)
    case GLongConstant(value) => LongLit(value)
    case GFloatConstant(value) => FloatLit(value)
    case GDoubleConstant(value) => DoubleLit(value)

    case GXor(op1, op2) => Bin(op1, "^", op2)
    case GOr(op1, op2) => Bin(op1, "|", op2)
    case GAnd(op1, op2) => Bin(op1, "&", op2)

    case GUshr(op1, op2) => Bin(Cast(ValueType("unsigned"), op1), ">>", op2)
    case GShr(op1, op2) => Bin(op1, ">>", op2)
    case GShl(op1, op2) => Bin(op1, "<<", op2)

    case GAdd(op1, op2) => Bin(op1, "+", op2)
    case GSub(op1, op2) => Bin(op1, "-", op2)
    case GMul(op1, op2) => Bin(op1, "*", op2)
    case GDiv(op1, op2) => Bin(op1, "/", op2)
    case GRem(op1, op2) => Bin(op1, "%", op2)

    case GEq(GCmpg(op1, op2), GIntConstant(0)) => Bin(op1, "==", op2)
    case GEq(GCmpl(op1, op2), GIntConstant(0)) => Bin(op1, "==", op2)
    case GEq(GCmp(op1, op2), GIntConstant(0)) => Bin(op1, "==", op2)
    case GEq(op1, op2) => Bin(op1, "==", op2)

    case GNe(GCmpg(op1, op2), GIntConstant(0)) => Bin(op1, "!=", op2)
    case GNe(GCmpl(op1, op2), GIntConstant(0)) => Bin(op1, "!=", op2)
    case GNe(GCmp(op1, op2), GIntConstant(0)) => Bin(op1, "!=", op2)
    case GNe(op1, op2) => Bin(op1, "!=", op2)

    case GGt(GCmpg(op1, op2), GIntConstant(0)) => Bin(op1, ">", op2)
    case GGt(GCmpl(op1, op2), GIntConstant(0)) => Bin(op1, ">", op2)
    case GGt(GCmp(op1, op2), GIntConstant(0)) => Bin(op1, ">", op2)
    case GGt(op1, op2) => Bin(op1, ">", op2)

    case GLt(GCmpg(op1, op2), GIntConstant(0)) => Bin(op1, "<", op2)
    case GLt(GCmpl(op1, op2), GIntConstant(0)) => Bin(op1, "<", op2)
    case GLt(GCmp(op1, op2), GIntConstant(0)) => Bin(op1, "<", op2)
    case GLt(op1, op2) => Bin(op1, "<", op2)

    case GLe(GCmpg(op1, op2), GIntConstant(0)) => Bin(op1, "<=", op2)
    case GLe(GCmpl(op1, op2), GIntConstant(0)) => Bin(op1, "<=", op2)
    case GLe(GCmp(op1, op2), GIntConstant(0)) => Bin(op1, "<=", op2)
    case GLe(op1, op2) => Bin(op1, "<=", op2)

    case GGe(GCmpg(op1, op2), GIntConstant(0)) => Bin(op1, ">=", op2)
    case GGe(GCmpl(op1, op2), GIntConstant(0)) => Bin(op1, ">=", op2)
    case GGe(GCmp(op1, op2), GIntConstant(0)) => Bin(op1, ">=", op2)
    case GGe(op1, op2) => Bin(op1, ">=", op2)

    case GCmpg(op1, op2) => Id("unimplemented:cmpg")
    case GCmpl(op1, op2) => Id("unimplemented:cmpl")
    case GCmp(op1, op2) => Id("unimplemented:cmp")

    case GNeg(op) => Un("-", op)

    // TODO - What do we do with this?  Does it need a representation in the C AST?
    case GArrayLength(op) => Id("unimplemented:arraylength")

    case GCast(op, castTyp) => Cast(translateType(castTyp), op)

    // IGNORE
    case GInstanceof(op, instTyp) => Id("unimplemented:instanceof")

    // IGNORE
    case GNew(newTyp) => Id("unimplemented:new")

    // IGNORE
    case GNewArray(newTyp, size) => Id("unimplemented:newarray")
    // IGNORE
    case GNewMultiArray(newTyp, sizes) => Id("unimplemented:newmultiarray")

    case GNewInvoke(baseTyp, method @ SMethodRef(_, "<init>", _, _, _), args) => {
      if (baseTyp.getSootClass.getName.equals("scala.Tuple2")) {
        Cast(StructType("Tuple2"), StructLit(args.map {
          // scala.runtime.BoxesRunTime.boxToFloat(float) : Object
          case v @ GStaticInvoke(SMethodRef(k @ SClassName("scala.runtime.BoxesRunTime"), "boxToInt", _, _, _), List(value)) => {
            val name = freshName("union")
            symtab.addLocalVar(ANY_TYPE, Id(name))
            Comma(Assign(Select(Id(name), "i"), translateExp(value)), Id(name))
          }
          case v @ GStaticInvoke(SMethodRef(k @ SClassName("scala.runtime.BoxesRunTime"), "boxToFloat", _, _, _), List(value)) => {
            val name = freshName("union")
            symtab.addLocalVar(ANY_TYPE, Id(name))
            Comma(Assign(Select(Id(name), "f"), translateExp(value)), Id(name))
          }
          case v => translateExp(v)
        }))
      }
      else if (isFunction(baseTyp)) {
        // worklist += CompileMethodTask(method)
        Call(Id("makeClosure"), args.map(a => translateExp(a)))
      }
      else {
        // worklist += CompileMethodTask(method)
        Call(Id("_init_"), Call(Id("new_" + mangleName(baseTyp.toString)), args.map(a => translateExp(a))))
      }
    }
    case GStaticInvoke(method, args) => {
      worklist += CompileMethodTask(method)
      Call(Id(method.name), args.map(a => translateExp(a)))
    }
    case GSpecialInvoke(base, method, args) => {
      worklist += CompileMethodTask(method, findSelf(base, symtab.self))
      Call(Select(base, method.name), args.map(a => translateExp(a)))
    }
    case GVirtualInvoke(base, method, args) => {
      worklist += CompileMethodTask(method, findSelf(base, symtab.self))

      // need to find all subclasses of method.getDeclaringClass that override method (i.e., have the same _.getSignature)
      // Then generate a call to a dispatch method:
      // e.g.,
      // class A { def m = ... }
      // class B extends A { def m = ... }
      // class C extends A { def m = ... }
      // val x: A = new C
      // x.m   // invokevirtual(x, A.m, Nil)
      // -->
      // dispatch_m(x)
      // where:
      // void dispatch_m(A* x) {
      //    switch (x.type) {
      //       case TYPE_A: A_m((A*) x);
      //       case TYPE_B: B_m((B*) x);
      //       case TYPE_C: C_m((C*) x);
      //    }
      // }
      //
      // Also need to define structs for each class with a type field as the first word.
      //
      // For now: just handle calls x.m() where we know either the static type
      // of x (e.g., A) is final, or m is final in A, or we know that no
      // subclasses of A override m
      
      // rewrite to:

      def getPossibleReceivers(base: Value, method: SootMethodRef) = {
        if (Modifier.isFinal(method.declaringClass.getModifiers)) {
          method.declaringClass :: Nil
        }
        else if (Modifier.isFinal(method.resolve.getModifiers)) {
          method.declaringClass :: Nil
        }
        else {
          base.getType match {
            case t : SootRefType if Modifier.isFinal(t.getSootClass.getModifiers) =>
              // assert method not overridden between method.declaringClass and t
              method.declaringClass :: Nil

            case t : SootRefType => {
              // iterate through all loaded subclasses of t, filtering out those that implement method
              val result = ListBuffer[SootClass]()

              val methodSig = method.name + soot.AbstractJasminClass.jasminDescriptorOf(method)
              val H = Scene.v.getActiveHierarchy

              val queue = new Queue[SootClass]()
              queue += t.getSootClass

              while (! queue.isEmpty) {
                val c = queue.dequeue

                def hasMethod(c: SootClass, methodSig: String): Boolean = {
                  val i = c.methodIterator
                  while (i.hasNext) {
                    val m = i.next

                    if (! m.isAbstract) {
                      val sig = m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef)
                      if (sig.equals(methodSig)) {
                        return true
                      }
                    }
                  }
                  return false
                }

                if (hasMethod(c, methodSig)) {
                  result += c
                }

                queue ++= H.getDirectSubclassesOf(c).asInstanceOf[java.util.List[SootClass]]toList
              }

              if (result.isEmpty)
                method.declaringClass :: Nil
              else 
                result.toList
            }

            case _ => Nil
          }
        }
      
    
      }

      val possibleReceivers = getPossibleReceivers(base, method)

      println("possibleReceiver(" + base + ", " + method + ") = " + possibleReceivers)

      assert(possibleReceivers.length > 0)

      if (possibleReceivers.length == 1) {
        // monomorphic call
        // should be: Call(Id(methodName(method)), translateExp(base)::args.map(a => translateExp(a)))
        Call(Id(methodName(method)), args.map(a => translateExp(a)))
      }
      else {
        // polymorphic call--generate a switch
        Call(Id("unimplemented: call to " + methodName(method)), TreeSeq())
      }
    
  
    }
    case GInterfaceInvoke(base, method, args) => {
      worklist += CompileMethodTask(method, findSelf(base, symtab.self))
      // need to find all subclasses of method.getDeclaringClass that override method (i.e., have the same _.getSignature)
      Call(Select(base, method.name), args.map(a => translateExp(a)))
    }

    case GLocal(name, typ) => { symtab.addLocalVar(typ, Id(mangleName(name))); Id(mangleName(name)) }
    case GThisRef(typ) => { symtab.addThisParam(typ, Id("_this")); Id("_this") }
    case GParameterRef(typ, index) => { symtab.addParamVar(typ, index, Id("_arg" + index)); Id("_arg" + index) }
    case GStaticFieldRef(fieldRef) => Id("unimplemented:staticfield")

    case GInstanceFieldRef(base, fieldRef) => Select(base, fieldRef.name)
    case GArrayRef(base, index) => ArrayAccess(base, index)

    case v => Id("unsupported:" + v.getClass.getName)
  }

  private def translateUnits(units: List[SootUnit], result: List[Tree]): List[Tree] = units match {
    case u::us => {
      val tree: Tree = u match {
        case GIdentity(left, right) => Eval(Assign(left, right))
        case GAssignStmt(left: Local, GNewArray(typ: SootArrayType, size)) => { symtab.locals -= Id(left.getName); symtab.addArrayDef(typ.getElementType, Id(left.getName), translateExp(size)); TreeSeq() }
        case GAssignStmt(left, right) => Eval(Assign(left, right))
        case GGoto(target) => GoTo(translateLabel(target))
        case GNop() => Nop
        case GReturnVoid() => Return
        case GReturn(op) => Return(translateExp(op)) match {
          case Return(Cast(typ, StructLit(fields))) => {
            val tmp = freshName("ret")
            symtab.addLocalVar(typ, Id(tmp))
            TreeSeq(Eval(Assign(Id(tmp), Cast(typ, StructLit(fields)))), Return(Id(tmp)))
          }
          case t => t
        }
        case GIf(cond, target) => If(translateExp(cond), GoTo(translateLabel(target)), Nop)
        case GInvokeStmt(invokeExpr) => Eval(translateExp(invokeExpr))

        // TODO
        case GTableSwitchStmt(key, lowIndex, highIndex, targets, defaultTarget) => Id("switch unsupported")
        case GLookupSwitchStmt(key: Local, lookupVals: List[Value], targets: List[Stmt], defaultTarget) => {
          val valsWithTargets: List[(Value, Stmt)] = lookupVals.zip(targets)
          Switch(translateExp(key), valsWithTargets.map(vt => Case(translateExp(vt._1), GoTo(translateLabel(vt._2)))) :::         List(Default(GoTo(translateLabel(defaultTarget)))))
          // Switch(Id(key.getName), valsWithTargets.map(vt => Case(translateExp(vt._1), TreeSeq(translateUnits(List(vt._2), Nil)))) ::: List(Default(TreeSeq(translateUnits(List(defaultTarget), Nil)))))
          //Id("switch unsupported")
        }
        // IGNORE
        case GThrow(op) => Id("throw unsupported")
        case GExitMonitor(op) => Id("monitors unsupported")
        case GEnterMonitor(op) => Id("monitors unsupported")

        case _ => { println("huh " + u); Id("unsupported") }
      }

      translateUnits(us, result ::: List[Tree](tree))
    }
    case Nil => result
  }

  private def insertLabels(units: List[SootUnit], result: List[Tree], resultWithLabels: List[Tree]) : List[Tree] = units match {
    case u::us => {
      symtab.labels.get(u) match {
        case Some(label) => insertLabels(us, result.tail, resultWithLabels ::: Label(label) :: result.head :: Nil)
        case None        => insertLabels(us, result.tail, resultWithLabels ::: result.head :: Nil)
      }
    }
   case Nil => resultWithLabels
  }

  private def makeFunction(m: SootMethod, result: List[Tree]) : Tree = {
    val paramTree = new ListBuffer[Tree]()
    val varTree = new ListBuffer[Tree]()

    for (i <- 0 until m.getParameterCount) {
      symtab.params.get(i) match {
        case Some((id, typ)) => paramTree += Formal(typ, id)
        case None => throw new RuntimeException("crap")
      }
    }

    for ((id: Id, typ: Tree) <- symtab.locals) 
      varTree += VarDef(typ, id)

    for (((id: Id), (typ: Tree, size: IntLit)) <- symtab.arrays)
      varTree += ArrayDef(id, typ, size)

    FunDef(translateType(m.getReturnType), Id(methodName(m)), paramTree.toList, (varTree.toList ::: result).toArray:_*)
  }
}
