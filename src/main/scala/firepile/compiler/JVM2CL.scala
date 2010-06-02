package firepile.compiler

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
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.toolkits.graph.UnitGraph
import soot.jimple.JimpleBody
import soot.jimple.Jimple
import soot.grimp.Grimp
import soot.grimp.GrimpBody
import soot.Type
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

import firepile.tree.Trees._
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
      println("usage: sooty.Main className methodName")
      exit(1)
    }

    val className = args(0)
    val methodName = args(1)

    compileRoot(className, methodName)
  }

    setup

  def compileRoot(className: String, methodName: String): List[Tree] = {
    println("compiling " + className + "." + methodName)
    try {
      addRootMethodToWorklist(className, methodName)
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

  def setup = {
    // java.class.path is broken in Scala, especially when running under sbt
    Scene.v.setSootClassPath(Scene.v.defaultClassPath
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0.RC3/classes"
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0.RC3/test-classes"
      + ":.:tests:bin:lib/soot-2.4.0.jar:/opt/local/share/scala-2.8/lib/scala-library.jar")

    // might be useful if you want to relate back to source code
    Options.v.set_keep_line_number(true)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.setPhaseOption("cg", "safe-forname:false")
    // you can set context-sensitivity of call graph with this (more time consuming)
    // Options.v.setPhaseOption("cg", "context:1cfa") 
    Options.v.setPhaseOption("cg", "verbose:true")

    Options.v.set_allow_phantom_refs(true)
    // Options.v.set_whole_program(true)
  }

  def mangleName(name: String) = name.replace('$', '_').replace('.', '_')

  implicit def v2tree(v: Value): Tree = translateExp(v)

  val worklist = new Queue[SootMethod]
  val inWorklist = new HashSet[SootMethod]

  private val makeCallGraph = false

  private def addRootMethodToWorklist(className: String, methodName: String): Unit = {
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
      if (sig.equals(methodName)) {
        enqueue(m)
      }
    }
  }

  private def enqueue(m: SootMethodRef): Unit = enqueue(m.resolve)

  private def enqueue(m: SootMethod): Unit = {
    if (! inWorklist.contains(m)) {
      println("enqueueing method" + m)
      worklist += m
      inWorklist += m
    }
  }

  private def buildCallGraph = {
    Scene.v.setEntryPoints(worklist.toList)
    println("entry points " + Scene.v.getEntryPoints)
    (new CallGraphBuilder).build
    println(Scene.v.getCallGraph)
  }

  private def optimizeCallGraph = {
    // Bind methods statically.
    StaticMethodBinder.v.transform
    StaticInliner.v.transform
  }

  def processWorklist = {
    val results = ListBuffer[Tree]()

    while (! worklist.isEmpty) {
      val m = worklist.dequeue
      val t = compileMethod(m)
      if (t != null)
          results += t
    }
    results.toList
  }

  def compileMethod(m: SootMethod): Tree = {
    symtab = new SymbolTable

    println("-------------------------------------------------------")
    println(m)

    if (m.isAbstract)
        return null
    if (m.isNative)
        return null

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
    val fun = makeFunction(m, prettify(removeThis(labeled)))
      // TODO: don't removeThis for normal methods; do removeThis for closures
      // TODO: verify that this is not used in the body of the method

    println()
    println("Result tree:")
    println(body.mkString("\n"))

    println()
    println("Result tree with labels:")
    println(labeled.mkString("\n"))

    println()
    println("Result tree with labels, params, local vars:")
    println(fun)

    println()
    println("Result CL:")
    println(fun.toCL)

    fun
  }

  def removeThis(body: List[Tree]): List[Tree] = {
    symtab.locals.remove(Id("this"))
    body match {
      case Eval(Assign(Id("this"), _))::ts => removeThis(ts)
      case t::ts => t::removeThis(ts)
      case Nil => Nil
    }
  }

  def not(t: Tree) = t match {
    case Bin(op1, "<", op2) => Bin(op1, ">=", op2)
    case Bin(op1, ">", op2) => Bin(op1, "<=", op2)
    case Bin(op1, "<=", op2) => Bin(op1, ">", op2)
    case Bin(op1, ">=", op2) => Bin(op1, "<", op2)
    case Bin(op1, "==", op2) => Bin(op1, "!=", op2)
    case Bin(op1, "!=", op2) => Bin(op1, "==", op2)
    case Un("!", op) => op
    case e => Un("!", e)
  }

  /*
  object IfThenElse {
    def unapply(ts: List[Tree]) = {
      ts match {
        case Nil => None
        case If(cond, GoTo(l0), Nop)::ts => {
          val (before,label,after) = ts.
                If(cond, 

  def prettify(body: List[Tree]): List[Tree] = body match {
    case If(cond, GoTo(l0), Nop)::t1::GoTo(l1)::Label(l0_)::t2::Label(l1_)::ts if l0 == l0_ && l1 == l1_ => prettify(If(not(cond), t1, Seq(Label(l0)::t2::Nil))::Label(l1)::ts)
    case t::ts => t::prettify(ts)
    case Nil => Nil
  }
*/

  def prettify(body: List[Tree]): List[Tree] = body

  class SymbolTable {
    val labels = new HashMap[SootUnit, String]()
    val params = new HashMap[Int, (Id, Type)]()
    val locals = new HashMap[Id, Type]()
    var thisParam: (Id, Type) = null

    def addThisParam(typ: Type, id: Id) = {
      thisParam = (id, typ)
    }

    def addParamVar(typ: Type, index: Int, id: Id) = {
      params += index -> (id, typ)
    }

    def addLocalVar(typ: Type, id: Id) = {
      assert(!params.contains(id))
      // assert(!locals.contains(id))
      locals += id -> typ
    }

    var next = -1

    def nextLabel = {
      next += 1
      next
    }
  }

  var symtab: SymbolTable = null

  def translateLabel(u: SootUnit): String = u match {
    case target : Stmt => {
      symtab.labels.get(target) match {
        case Some(label) => label
        case None => {
          val label = "lbl_" + symtab.nextLabel
          symtab.labels += target -> label
          label
        }
      }
    }
    case _ => "Label"
  }


  def translateType(t: Type): Tree = t match {
    case t : SootVoidType => ValueType("void")
    case t : SootBooleanType => ValueType("int")
    case t : SootByteType => ValueType("char")
    case t : SootShortType => ValueType("short")
    case t : SootCharType => ValueType("ushort")
    case t : SootIntType => ValueType("int")
    case t : SootLongType => ValueType("long")
    case t : SootFloatType => ValueType("float")
    case t : SootDoubleType => ValueType("double")
    case t : SootRefType => PtrType(ValueType(mangleName(t.toString)))
    // TODO: array types
    case _ => ValueType(t.toString)
  }

  object MathCall {
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
      case MathCall(name, List(GCast(FloatTyped(x), d))) if d.equals(SootDoubleType.v) => Some((name, List(x)))
      case _ => None
    }
  }

  // Split library calls into separate object
  // This avoids an OutOfMemory error in scalac.
  object MathLibraryCall {
    def unapply(v: Value) = {
      val t: Tree = v match {
        // [NN] Many of these translations aren't right!  I was
        // translating to functions in math.h, but these are often
        // different than the math functions supported by OpenCL.
        case MathCall("toRadians", List(DoubleTyped(x))) => Bin(Bin(x, "*", Id("M_PI")), "/", DoubleLit(180.))
        case MathCall("toDegrees", List(DoubleTyped(x))) => Bin(Bin(x, "*", DoubleLit(180.)), "/", Id("M_PI"))
        case MathCall("exp", List(FloatTyped(x))) => Call(Id("expf"), x)
        case MathCall("exp", List(DoubleTyped(x))) => Call(Id("exp"), x)
        case MathCall("log", List(FloatTyped(x))) => Call(Id("logf"), x)
        case MathCall("log", List(DoubleTyped(x))) => Call(Id("log"), x)
        case MathCall("sqrt", List(FloatTyped(x))) => Call(Id("sqrtf"), x)
        case MathCall("sqrt", List(DoubleTyped(x))) => Call(Id("sqrt"), x)
        case MathCall("IEEEremainder", List(DoubleTyped(x), DoubleTyped(y))) => Call(Id("fmod"), y, x)
        case MathCall("ceil", List(FloatTyped(x))) => Call(Id("ceilf"), x)
        case MathCall("ceil", List(DoubleTyped(x))) => Call(Id("ceil"), x)
        case MathCall("floor", List(FloatTyped(x))) => Call(Id("floorf"), x)
        case MathCall("floor", List(DoubleTyped(x))) => Call(Id("floor"), x)
        case MathCall("rint", List(FloatTyped(x))) => Call(Id("rintf"), x)
        case MathCall("rint", List(DoubleTyped(x))) => Call(Id("rint"), x)
        case MathCall("atan2", List(FloatTyped(y), FloatTyped(x))) => Call(Id("atan2f"), y, x)
        case MathCall("atan2", List(DoubleTyped(y), DoubleTyped(x))) => Call(Id("atan2"), y, x)
        case MathCall("pow", List(FloatTyped(y), FloatTyped(x))) => Call(Id("powf"), y, x)
        case MathCall("pow", List(DoubleTyped(y), DoubleTyped(x))) => Call(Id("pow"), y, x)
        case MathCall("round", List(FloatTyped(x))) => Cast(ValueType("int"), Call(Id("lroundf"), x))
        case MathCall("round", List(DoubleTyped(x))) => Call(Id("llround"), x)

          // Need similar for longs
        case MathCall("abs", List(IntTyped(x))) => Call(Id("abs"), x)
        case MathCall("abs", List(FloatTyped(x))) => Call(Id("fabsf"), x)
        case MathCall("abs", List(DoubleTyped(x))) => Call(Id("fabs"), x)

        // Need similar for longs
        case MathCall("max", List(IntTyped(y), IntTyped(x))) => Call(Id("max"), y, x)
        case MathCall("max", List(FloatTyped(y), FloatTyped(x))) => Call(Id("fmaxf"), y, x)
        case MathCall("max", List(DoubleTyped(y), DoubleTyped(x))) => Call(Id("fmax"), y, x)

        // Need similar for longs
        case MathCall("min", List(IntTyped(y), IntTyped(x))) => Call(Id("min"), y, x)
        case MathCall("min", List(FloatTyped(y), FloatTyped(x))) => Call(Id("fminf"), y, x)
        case MathCall("min", List(DoubleTyped(y), DoubleTyped(x))) => Call(Id("fmin"), y, x)

        case MathCall("signum", List(x)) => Call(Id("SIGNUM"), x)

        case _ => null
      }
      if (t == null) None else Some(t)
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

  
    object IntrinsicCall {
        def apply(returnType: Type, containerType: Type, fun: Tree, args: Tree*): IntrinsicCall = IntrinsicCall(returnType, containerType, fun, args.toList)
    }
    case class IntrinsicCall(returnType: Type, containerType: Type, fun: Tree, args: List[Tree]) extends Tree {
        def toCL = "intrinsic[" + containerType + "::" + fun.toCL + ": " + returnType + "]" + args.map((t:Tree) => t.toCL).mkString("(", ", ", ")")
    }

  object CollectionCall {
    def unapply(v: Value): Option[Tree] = {println("coll? " + v); v} match {
      // staticinvoke <scala.runtime.BoxesRunTime: float unboxToFloat(java.lang.Object)>(obj)
      case v @ GStaticInvoke(SMethodRef(k @ SClassName("scala.runtime.BoxesRunTime"), name, _, _, _), List(obj)) => {
        val x = Some(IntrinsicCall(v.getType, k.getType, Id(name), translateExp(obj)))
        println("returning coll " + x)
        x
      }
      case v @ GInterfaceInvoke(coll, SMethodRef(k @ SClassName("scala.collection.IndexedSeqOptimized"), "zip", _, _, _), List(coll2, cbf)) => {
        val x = Some(IntrinsicCall(v.getType, k.getType, Id("zip"), coll, coll2))
        println("returning coll " + x)
        x
      }
      case v @ GInterfaceInvoke(coll, SMethodRef(k @ SClassName("scala.collection.IndexedSeqOptimized"), name, _, _, _), args) => {
        val x = Some(IntrinsicCall(v.getType, k.getType, Id(name), (coll::args).map(a => translateExp(a))))
        println("returning coll " + x)
        x
      }
      case v @ GInterfaceInvoke(coll, SMethodRef(k @ SClassName("scala.collection.TraversableLike"), "map", _, _, _), List(fun, cbf)) => {
        val x = Some(IntrinsicCall(v.getType, k.getType, Id("map"), coll, fun))
        println("returning coll " + x)
        x
      }
      case v @ GInterfaceInvoke(coll, SMethodRef(k @ SClassName("scala.collection.TraversableLike"), name, _, _, _), args) => {
        val x = Some(IntrinsicCall(v.getType, k.getType, Id(name), (coll::args).map(a => translateExp(a))))
        println("returning coll " + x)
        x
      }
      case GVirtualInvoke(coll, SMethodRef(klass, name, _, _, _), args) => {
        println("virtual call with klass=" + klass.getName + " name=" + name + " args=" + args.toList)
        None
      }
      case GInterfaceInvoke(coll, SMethodRef(klass, name, _, _, _), args) => {
        println("interface call with klass=" + klass.getName + " name=" + name + " args=" + args.toList)
        None
      }
      case GCast(e, t : SootRefType) if t.getClassName.equals("firepile.util.BufferBackedArray$BBArray") => Some(translateExp(e))
      case _ => {
        println("did not match " + v.getClass.getName)
        None
      }
    }
    /*
    b = (firepile.util.BufferBackedArray$BBArray) this.<firepile.tests.TestDotProduct$$anonfun$3$$anonfun$4: firepile.tests.TestDotProduct$$anonfun$3 $outer>.<firepile.tests.TestDotProduct$$anonfun$3: firepile.util.BufferBackedArray$BBArray b1$1>.<scala.collection.IndexedSeqOptimized: java.lang.Object zip(scala.collection.Iterable,scala.collection.generic.CanBuildFrom)>(this.<firepile.tests.TestDotProduct$$anonfun$3$$anonfun$4: firepile.tests.TestDotProduct$$anonfun$3 $outer>.<firepile.tests.TestDotProduct$$anonfun$3: firepile.util.BufferBackedArray$BBArray b2$1>, <firepile.util.BufferBackedArray$: firepile.util.BufferBackedArray$ MODULE$>.<firepile.util.BufferBackedArray$: scala.collection.generic.CanBuildFrom bbarrayCBFromBBArray(firepile.util.BufferBackedArray$FixedSizeMarshal)>(<firepile.util.BufferBackedArray$: firepile.util.BufferBackedArray$ MODULE$>.<firepile.util.BufferBackedArray$: firepile.util.BufferBackedArray$T2M tuple2Marshal(firepile.util.BufferBackedArray$FixedSizeMarshal,firepile.util.BufferBackedArray$FixedSizeMarshal)>(<firepile.util.BufferBackedArray$FM$: firepile.util.BufferBackedArray$FM$ MODULE$>, <firepile.util.BufferBackedArray$FM$: firepile.util.BufferBackedArray$FM$ MODULE$>)))
    f = new firepile.tests.TestDotProduct$$anonfun$3$$anonfun$4$$anonfun$5(this)
    m = (firepile.util.BufferBackedArray$BBArray) b.<scala.collection.TraversableLike: java.lang.Object map(scala.Function1,scala.collection.generic.CanBuildFrom)>(f, <firepile.util.BufferBackedArray$: firepile.util.BufferBackedArray$ MODULE$>.<firepile.util.BufferBackedArray$: scala.collection.generic.CanBuildFrom bbarrayCBFromBBArray(firepile.util.BufferBackedArray$FixedSizeMarshal)>(<firepile.util.BufferBackedArray$FM$: firepile.util.BufferBackedArray$FM$ MODULE$>))
    r = staticinvoke <scala.runtime.BoxesRunTime: float unboxToFloat(java.lang.Object)>(m.<scala.collection.IndexedSeqOptimized: java.lang.Object reduceLeft(scala.Function2)>(new firepile.tests.TestDotProduct$$anonfun$3$$anonfun$4$$anonfun$6(this)))
    return r
    */

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


        case DoubleMath(t) => t
        case FloatMath(t) => t
        case CollectionCall(t) => t
        // case MathLibraryCall(t) => t

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

  def translateExp(v: Value): Tree = v match {
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

    // TODO
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
      enqueue(method)

      def isFunctionClass(k: SootClass): Boolean = {
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

      def isFunction(t: Type): Boolean = t match {
        case t : SootRefType => t.hasSootClass && isFunctionClass(t.getSootClass)
        case _ => false
      }

      if (isFunction(baseTyp))
        IntrinsicCall(baseTyp, baseTyp, Id("makeClosure"), args.map(a => translateExp(a)))
      else
        Call(Id("_init_"), IntrinsicCall(baseTyp, baseTyp, Id("new"), args.map(a => translateExp(a))))
    }
    case GStaticInvoke(method, args) => {
      enqueue(method)
      Call(Id(method.name), args.map(a => translateExp(a)))
    }
    case GSpecialInvoke(base, method, args) => {
      enqueue(method)
      Call(Select(base, method.name), args.map(a => translateExp(a)))
    }
    case GVirtualInvoke(base, method, args) => {
      enqueue(method)
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
      // Call(mangle(method.getDeclaringClass, method.name), (base::args).map(a => translateExp(a)))
      Call(Select(base, method.name), args.map(a => translateExp(a)))
    }
    case GInterfaceInvoke(base, method, args) => {
      enqueue(method)
      // need to find all subclasses of method.getDeclaringClass that override method (i.e., have the same _.getSignature)
      Call(Select(base, method.name), args.map(a => translateExp(a)))
    }

    case GLocal(name, typ) => { symtab.addLocalVar(typ, Id(mangleName(name))); Id(mangleName(name)) }
    case GThisRef(typ) => { symtab.addThisParam(typ, Id("_this")); Id("_this") }
    case GParameterRef(typ, index) => { symtab.addParamVar(typ, index, Id("_arg" + index)); Id("_arg" + index) }
    case GStaticFieldRef(fieldRef) => Id("unimplemented:staticfield")

    case GInstanceFieldRef(base, fieldRef) => Select(base, fieldRef.name)
    case GArrayRef(base, index) => ArrayAccess(base, index)

    case v => { println("huh " + v); Id("unsupported") }
  }

  def translateUnits(units: List[SootUnit], result: List[Tree]): List[Tree] = units match {
    case u::us => {
      val tree: Tree = u match {
        case GIdentity(left, right) => Eval(Assign(left, right))
        case GAssignStmt(left, right) => Eval(Assign(left, right))
        case GGoto(target) => GoTo(translateLabel(target))
        case GNop() => Nop
        case GReturnVoid() => Return
        case GReturn(op) => Return(translateExp(op))
        case GIf(cond, target) => If(translateExp(cond), GoTo(translateLabel(target)), Nop)
        case GInvokeStmt(invokeExpr) => Eval(translateExp(invokeExpr))

        // TODO
        case GTableSwitchStmt(key, lowIndex, highIndex, targets, defaultTarget) => Id("switch unsupported")
        case GLookupSwitchStmt(key, lookupVals, targets, defaultTarget) => Id("switch unsupported")

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

  def insertLabels(units: List[SootUnit], result: List[Tree], resultWithLabels: List[Tree]) : List[Tree] = units match {
    case u::us => {
      symtab.labels.get(u) match {
        case Some(label) => insertLabels(us, result.tail, resultWithLabels ::: Label(label) :: result.head :: Nil)
        case None        => insertLabels(us, result.tail, resultWithLabels ::: result.head :: Nil)
      }
    }
   case Nil => resultWithLabels
  }

  def makeFunction(m: SootMethod, result: List[Tree]) : Tree = {
    val paramTree = new ListBuffer[Tree]()
    val varTree = new ListBuffer[Tree]()

    /*
    if (! m.isStatic) {
      assert(symtab.thisParam != null)
      val (id,typ) = symtab.thisParam
      paramTree += Formal(translateType(typ), id)
    }
    */

    for (i <- 0 until m.getParameterCount) {
      symtab.params.get(i) match {
        case Some((id, typ)) => paramTree += Formal(translateType(typ), id)
        case None => throw new RuntimeException("crap")
      }
    }

    for((id: Id, typ: Type) <- symtab.locals)
      varTree += VarDef(translateType(typ), id)

    FunDef(translateType(m.getReturnType), Id(mangleName(m.getName)), paramTree.toList, (varTree.toList ::: result).toArray:_*)
  }
}
