package firepile.compiler

import scala.tools.scalap._
import scala.tools.scalap.{ Main => Scalap }
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
import soot.{ Unit => SootUnit }
import soot.Scene
import soot.Value
import soot.ValueBox
import soot.Local
import soot.SootClass
import soot.SootMethod
import soot.SootMethodRef
import soot.Modifier
import soot.RefType
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.toolkits.graph.UnitGraph
import soot.Hierarchy
import soot.jimple.JimpleBody
import soot.jimple.Jimple
import soot.grimp.Grimp
import soot.grimp.GrimpBody
import soot.grimp.internal.GVirtualInvokeExpr
import soot.grimp.internal.{GInstanceFieldRef => SootGInstanceFieldRef}
import soot.{ Type => SootType }
import soot.jimple.Stmt
import soot.{ VoidType => SootVoidType }
import soot.{ BooleanType => SootBooleanType }
import soot.{ ByteType => SootByteType }
import soot.{ ShortType => SootShortType }
import soot.{ CharType => SootCharType }
import soot.{ IntType => SootIntType }
import soot.{ LongType => SootLongType }
import soot.{ FloatType => SootFloatType }
import soot.{ DoubleType => SootDoubleType }
import soot.{ RefType => SootRefType }
import soot.{ ArrayType => SootArrayType }
import soot.{ NullType => SootNullType }

import firepile.compiler.util.TypeGen.getSignature
import firepile.compiler.util.ScalaTypeGen
import firepile.compiler.util.ScalaTypeGen.{
  getScalaSignature,
  getScalaJavaSignature,
  printClassDef
}
import firepile.compiler.util.JavaTypeGen.getJavaSignature
import firepile.compiler.util.ClassDefs.{
  ScalaClassDef,
  ScalaVarDef,
  NamedTyp,
  InstTyp
}
import firepile.compiler.util.TypeFlow.getSupertypes
import firepile.tree.Trees._
import firepile.Kernel
import firepile.tree.Trees.{ Seq => TreeSeq }
import scala.Seq
import soot.jimple.{
  FloatConstant,
  DoubleConstant,
  IntConstant,
  LongConstant,
  StringConstant
}
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

    val tree = compileRoot(className, methodSig)

    println("Printing tree")
    for (t <- tree) println(t.toCL)

    tree
  }

  private val makeCallGraph = true
  private var activeHierarchy: Hierarchy = null
  private var ids = Array(false, false, false)

  setup

  def compileRoot(className: String, methodSig: String): List[Tree] = {
    println("compiling " + className + "." + methodSig)
    try {
      addRootMethodToWorklist(className, methodSig)
      if (makeCallGraph) {
        buildCallGraph
        optimizeCallGraph
      }
      println("before process work list")
      val proc = processWorklist
      println("after process work list")

      println("Result CL:")
      for (t <- proc) println(t.toCL)
      proc
    } catch {
      case e: ClassNotFoundException => {
        println("Class not found: " + e.getMessage)
        e.printStackTrace
        Nil
      }
    }
  }

  def compileMethod(className: String, methodSig: String): List[Tree] = {
    println("compiling " + className + "." + methodSig)
    try {
      addMethodToWorklist(className, methodSig)
      if (makeCallGraph) {
        buildCallGraph
        optimizeCallGraph
      }
      println("before process work list")
      val proc = processWorklist
      println("after process work list")

      println("Result CL:")
      for (t <- proc) println(t.toCL)
      proc
    } catch {
      case e: ClassNotFoundException => {
        println("Class not found: " + e.getMessage)
        e.printStackTrace
        Nil
      }
    }
  }

  private def setup = {
    // java.class.path is broken in Scala, especially when running under sbt
    //Scene.v.setSootClassPath(Scene.v.defaultClassPath
    //println("setting up"+System.getProperty("os.name"))

    if (System.getProperty("os.name").toLowerCase().startsWith("win"))
      Scene.v.setSootClassPath(Scene.v.defaultClassPath
        + ";." + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/firepilesoot.jar"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/firepilesootest.jar"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/soot-2.4.0.jar"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/scalap.jar"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/rt.jar"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/jce.jar"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/jasminsrc-2.4.0.jar"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/scala-compiler.jar"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/target/classes"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/target/testclasses"
        + ";C:/ScalaWorld/CompleteFirepileCompiler/lib/scala-library.jar")
    else
      Scene.v.setSootClassPath(Scene.v.defaultClassPath
        + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0-local/classes"
        + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0-local/test-classes"
        + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0.RC3/classes"
        + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0.RC3/test-classes"
        + ":/Users/nystrom/firepile/target/scala_2.8.0.RC3/classes"
        + ":/Users/nystrom/firepile/target/scala_2.8.0.RC3/test-classes"
        + ":/Users/dwhite/git3/firepile/target/scala_2.8.0-local/classes"
        + ":/Users/dwhite/git3/firepile/target/scala_2.8.0-local/test-classes"
        + ":/Users/dwhite/opt/scala-2.8.0.final/lib/scala-library.jar"
        + ":.:tests:examples:tests/VirtualInvoke:bin:lib/soot-2.4.0.jar:/opt/local/share/scala-2.8/lib/scala-library.jar")

    // Manually add basic classes to scene for testing VirtualInvoke
    Scene.v.addBasicClass("VirtualInvokeA")
    Scene.v.addBasicClass("VirtualInvokeB")
    Scene.v.addBasicClass("VirtualInvokeC")
    Scene.v.addBasicClass("VirtualInvokeX")
    Scene.v.addBasicClass("Point")
    Scene.v.addBasicClass("Point1D")
    Scene.v.addBasicClass("Point2D")
    Scene.v.addBasicClass("Point3D")

    // might be useful if you want to relate back to source code
    Options.v.set_keep_line_number(true)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.setPhaseOption("cg", "safe-forname:false")
    // you can set context-sensitivity of call graph with this (more time consuming)
    // Options.v.setPhaseOption("cg", "context:1cfa")
    //Options.v.setPhaseOption("cg", "verbose:true")

    Options.v.set_allow_phantom_refs(true)
    if (makeCallGraph)
      Options.v.set_whole_program(true)

    activeHierarchy = Scene.v.getActiveHierarchy

  }

  def methodName(m: SootMethod): String = mangleName(m.getDeclaringClass.getName + m.getName)
  def methodName(m: java.lang.reflect.Method): String = mangleName(m.getDeclaringClass.getName + m.getName)
  def methodName(m: SootMethodRef): String = mangleName(m.declaringClass.getName + m.name)
  def mangleName(name: String): String = name.replace(' ', '_').replace('$', '_').replace('.', '_')

  private implicit def v2tree(v: Value)(implicit iv: (SymbolTable, HashMap[String, Value]) = null): Tree = translateExp(v, iv._1, iv._2)

  var next = 0
  def freshName(base: String = "tmp") = {
    next += 1
    base + next
  }

  class Worklist[A] extends Queue[A] {
    val inWorklist = new HashSet[A]

    override def +=(a: A) = {
      if (!inWorklist.contains(a)) {
        inWorklist += a
        super.+=(a)
      } else
        this
    }
  }

  trait Task {
    def run: List[Tree]
    def method: SootMethodRef
  }

  object CompileMethodTask {
    def apply(m: SootMethod, takesThis: Boolean = false, anonFuns: List[(Int, Value)] = null): CompileMethodTask = CompileMethodTask(m.makeRef, takesThis, anonFuns)
    def apply(m: SootMethodRef, takesThis: Boolean): CompileMethodTask = CompileMethodTask(m, takesThis, null)
    def apply(m: SootMethodRef): CompileMethodTask = CompileMethodTask(m, false, null)
  }

  case class CompileMethodTree(t: Tree) extends Task {
    def run = {
      t match {
        case FunDef(_, name, _, _) if name.startsWith("firepile_util_Unsigned") => Nil
        case _ => List(compileMethod(t))
      }
    }

    def method = null
  }

  case class CompileMethodTask(method: SootMethodRef, takesThis: Boolean, anonFuns: List[(Int, Value)]) extends Task {
    def run = {
      val m = method
      val anonFunsLookup = new HashMap[String, Value]()

      // Force the class's method bodies to be loaded.
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.HIERARCHY)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.SIGNATURES)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.BODIES)

      if (anonFuns != null) {
        for (af <- anonFuns) {
          val (index, value) = af
          // println("FUNCTION TYPE: " + value.getType.toString + " IS ARG OF " + method.resolve.getDeclaringClass)
          val cdef = getScalaSignature(method.resolve.getDeclaringClass.toString.replaceAll("\\$", "")).head
          val mdef = cdef.methods.filter(m => m.name.equals(method.name)).head
          val paramName = mdef.params(index).name
          // println("FUNCTION PARAM IS NAMED: " + paramName)

          anonFunsLookup += paramName -> value
        }
      }

      println("TASK RUN ON METHOD NAME: " + m.name + " declaring Class::" + m.declaringClass.getName)
      // m.declaringClass.getName.startsWith("init") removed 
      if (m.declaringClass.getName.startsWith("java.lang") || m.declaringClass.getName.startsWith("firepile.Spaces") || m.declaringClass.getName.startsWith("scala.runtime") || m.declaringClass.getName.startsWith("scala.Product") || m.declaringClass.getName.startsWith("firepile.util.Unsigned"))
        Nil
      else {
        // println("RETURNING BODY FOR METHOD NAME: " + m.name)
        compileMethod(m.resolve, 0, takesThis, anonFunsLookup) match {
          case null => Nil
          case t => t :: Nil
        }
      }
    }
  }

  case class CompileRootMethodTask(method: SootMethodRef, takesThis: Boolean, anonFuns: List[(Int, Value)]) extends Task {
    def run = {
      val m = method
      val anonFunsLookup = new HashMap[String, Value]()
      kernelMethod = true

      // Force the class's method bodies to be loaded.
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.HIERARCHY)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.SIGNATURES)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.BODIES)

      if (anonFuns != null) {
        for (af <- anonFuns) {
          val (index, value) = af
          // println("FUNCTION TYPE: " + value.getType.toString + " IS ARG OF " + method.resolve.getDeclaringClass)
          val cdef = getScalaSignature(method.resolve.getDeclaringClass.toString.replaceAll("\\$", "")).head
          val mdef = cdef.methods.filter(m => m.name.equals(method.name)).head
          val paramName = mdef.params(index).name
          // println("FUNCTION PARAM IS NAMED: " + paramName)

          anonFunsLookup += paramName -> value
        }
      }

      compileMethod(m.resolve, 0, takesThis, anonFunsLookup) match {
        case null => Nil
        case t => {
          val popArrayStructs = ListBuffer[Tree]()
          // Convert array struct into raw type and length params for root
          val frmls = t.formals.filter(f => f match {
            case Formal(StructType(typ), _) if typ.startsWith("firepile_Spaces_Id") => false
            case _ => true
          }).flatMap(f => f match {
            case Formal(StructType(typ), name) if typ.endsWith("Array") => {
              var rawTypeName = typ.substring(typ.indexOf('_') + 1, typ.lastIndexOf("Array"))
              if (rawTypeName.contains("unsigned_int")) rawTypeName = "unsigned int"
              typ match {
                case x if x.startsWith("g") => { // handle the global arrays
                  // popArrayStructs += VarDef(StructType(typ), Id(name))
                  popArrayStructs += Assign(Select(Select(Id("_this_kernel"), Id(name)), Id("data")), Id(name + "_data"))
                  popArrayStructs += Assign(Select(Select(Id("_this_kernel"), Id(name)), Id("length")), Id(name + "_len"))
                  List(Formal(MemType("global", PtrType(ValueType(rawTypeName))), name + "_data"), Formal(ConstType(IntType), name + "_len"))
                }
                case x if x.startsWith("l") => { // handle the local arrays
                  // popArrayStructs += VarDef(StructType(typ), Id(name))
                  popArrayStructs += Assign(Select(Select(Id("_this_kernel"), Id(name)), Id("data")), Id(name + "_data"))
                  popArrayStructs += Assign(Select(Select(Id("_this_kernel"), Id(name)), Id("length")), Id(name + "_len"))
                  List(Formal(MemType("local", PtrType(ValueType(rawTypeName))), name + "_data"), Formal(ConstType(IntType), name + "_len"))
                }
                case x => Nil
              }
            }
            case x => List(x)
          })
          KernelFunDef(Id(t.name), frmls, TreeSeq(popArrayStructs.toList), t.body) :: Nil
        }
      }
    }
  }

  private val worklist = new Worklist[Task]

  private def addRootMethodToWorklist(className: String, methodSig: String): Unit = {
    println("\n\nADD ROOT METHOD TO WORKLIST\n\n")
    // Set up the class we're working with
    val c = Scene.v.loadClassAndSupport(className)
    if (makeCallGraph) {
      Scene.v.loadNecessaryClasses
      c.setApplicationClass
    }

    // Retrieve the method and its body
    // println(" Method Iterator ")
    for (m <- c.methodIterator) {
      // println("m.getName" + m.getName)
      val sig = m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef)
      // println("trying " + sig)
      if (sig.equals(methodSig)) {
        worklist += CompileRootMethodTask(m.makeRef, false, null)
      }
    }
  }

  private def addMethodToWorklist(className: String, methodSig: String): Unit = {
    // Set up the class we're working with
    val c = Scene.v.loadClassAndSupport(className)
    if (makeCallGraph) {
      Scene.v.loadNecessaryClasses
      c.setApplicationClass
    }

    // Retrieve the method and its body
    // println(" Method Iterator ")
    for (m <- c.methodIterator) {
      // println("m.getName" + m.getName)
      val sig = m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef)
      // println("trying " + sig)
      if (sig.equals(methodSig)) {
        worklist += CompileMethodTask(m.makeRef, false, null)
      }
    }
  }

  private def buildCallGraph = {
    Scene.v.setEntryPoints(worklist.toList.map(p => p.method.resolve))
    // println("entry points " + Scene.v.getEntryPoints)
    (new CallGraphBuilder).build
    // println(Scene.v.getCallGraph)
  }

  private def optimizeCallGraph = {
    // Bind methods statically.
    StaticMethodBinder.v.transform
    StaticInliner.v.transform
  }

  private def processWorklist = {
    val results = ListBuffer[Tree]()
    val functionDefs = HashMap[String, Tree]()
    val preamble = ListBuffer[Tree]()

    while (!worklist.isEmpty) {
      val task = worklist.dequeue
      val ts = task.run
      ts match {
        case FunDef(PtrType(ValueType("scala_Function2")), n, f, b) :: fs => {
          if (!functionDefs.contains(n) && !n.equals("firepile_Spaces_Point1<init>")) {
            functionDefs += n -> FunDec(VoidType, n, f)
            results ++= FunDef(VoidType, n, f, b) :: fs
          }
        }
        case FunDef(t, n, f, _) :: fs => {
          if (!functionDefs.contains(n) && !n.equals("firepile_Spaces_Point1<init>")) {
            functionDefs += n -> FunDec(t, n, f)
            results ++= ts
          }
        }
        case _ => results ++= ts
      }
    }

    preamble += Id("#define NULL 0L\n")
    preamble += Id("#define makeClosure(x) NULL\n")
    // preamble += Id("#define boxToFloat(x) x")
    // preamble += Id("#define unboxToFloat(x) x")
    preamble += Id("typedef int scala_Function2;\n")

    // group/item description structs added to environment struct set and appened to preamble
    envstructs.structs += ValueType("firepile_Group") ->
      List(StructDef("firepile_Group", List(VarDef(IntType, Id("id")), VarDef(IntType, Id("size")))))
    envstructs.structs += ValueType("firepile_Item") ->
      List(StructDef("firepile_Item", List(VarDef(IntType, Id("id")), VarDef(IntType, Id("size")), VarDef(IntType, Id("globalId")))))

    val preamblePostStructs = new ListBuffer[Tree]()
    preamblePostStructs += VarDef(StructType("firepile_Group"), Id("_group_desc"))
    preamblePostStructs += VarDef(StructType("firepile_Item"), Id("_item_desc"))
    preamblePostStructs += VarDef(StructType("kernel_ENV"), Id("_this_kernel"))


    val tree = preamble.toList ::: /* classtab.dumpClassTable ::: */ arraystructs.dumpArrayStructs ::: envstructs.dumpEnvStructs ::: preamblePostStructs.toList ::: functionDefs.values.toList ::: results.toList

    arraystructs.clearArrays
    envstructs.clearEnvs
    classtab.clearClassTable

    /*
     println()
     println("Unformatted tree:::\n"+tree)
     println("Formatted Result tree:")
     printTree(tree.asInstanceOf[scala.Product],0)

     println()
     println("Result CL:::")
     for (t <- tree) println(t.toCL)
     */
    tree
  }

  def printTree(a: scala.Product, indent: Int): Unit = {
    for (i <- a.productIterator) {
      try {
        i match {
          case l: List[Any] => println(); for (j <- l.asInstanceOf[List[scala.Product]]) printTree(j, indent + 2)
          case TreeSeq(tree: Tree) => printTree(tree.asInstanceOf[scala.Product], indent + 2)
          case TreeSeq(list: List[Tree]) => println(); for (i <- list) printTree(i.asInstanceOf[scala.Product], indent + 2)
          case TreeSeq(_) => printIndent(indent, i)
          case Seq(a) => println(); for (j <- i.asInstanceOf[Seq[scala.Product]]) printTree(j, indent + 2)
          case _ => printIndent(indent, i)

        }
      } catch {
        case e: ClassCastException => printIndent(indent, i)
        case _ => printIndent(indent, i)
      }

    }
  }

  def printIndent(indent: Int, item: Any): Unit = {
    println("")
    for (i <- 0 to indent)
      print(" ")
    print(item)
  }

  def isStatic(flags: Int) = (flags & 0x0008) != 0
  def flagsToStr(clazz: Boolean, flags: Int): String = {
    val buffer = new StringBuffer()
    var x: StringBuffer = buffer
    if (((flags & 0x0007) == 0) && ((flags & 0x0002) != 0))
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

  private def compileMethod(m: SootMethod, level: Int, takesThis: Boolean, anonFuns: HashMap[String, Value]): FunDef = {
    // println("-------------------------------------------------------")
    // println(m)

    if (m.isAbstract)
      return null
    if (m.isNative)
      return null

    val symtab = new SymbolTable(methodName(m))
    symtab.level = level

    if (kernelMethod) {
      symtab.kernelMethod = true
      kernelMethod = false
    }

    val b = m.retrieveActiveBody
    val gb = Grimp.v.newBody(b, "gb")

    val unitBuffer = ListBuffer[SootUnit]()
    for (u <- gb.getUnits) {
      unitBuffer += u
    }

    val units = unitBuffer.toList
    println("Grimp method body:")
    println(units.mkString("\n"))

    val body = translateUnits(units, Nil, symtab, anonFuns)

    if (Kernel.level == 2)
      return null
    else if (Kernel.level == 3) {
      Kernel.level = 4
      return null

    }

    val labeled = insertLabels(units, symtab, body, Nil)

    val fun = makeFunction(m, removeThis(labeled, symtab), symtab, false)

    // TODO: don't removeThis for normal methods; do removeThis for closures
    // TODO: verify that this is not used in the body of the method

    // printlns commented out to only print when processing worklist

    /*
    println()
    println("Result tree:")
    println(fun)

    println()
    println("Result CL:::")
    println(fun.toCL)
    */

    fun
  }

  def replaceClosures(funLookup: HashMap[String, Id], body: List[Tree]): List[Tree] = {
    body.map {
      /*
      case ClosureCall(id: Id, args) => Call(funLookup(id.name), args)
      case TreeSeq(tts) => TreeSeq(replaceClosures(funLookup, tts))
      case t => { println("DIDNT MATCH: " + t); t}
      */
      fold({
        case ClosureCall(id: Id, args) => Call(funLookup(id.name), args)
        case t => t
      })(_)
    }
  }

  private def compileMethod(m: Tree): Tree = {
    /*
    println("-------------------------------------------------------")

    println()
    println("Result tree:")
    println(m)

    println()
    println("Result CL:")
    println(m.toCL)
    */
    m
  }

  private def removeThis(body: List[Tree], symtab: SymbolTable): List[Tree] = {
    body match {
      case Eval(Assign(v: Id, Id("_this"))) :: ts => {
        symtab.locals.remove(v)
        removeThis(ts, symtab)
      }
      case t :: ts => t :: removeThis(ts, symtab)
      case Nil => Nil
    }
  }

  val ANY_TYPE = ValueType("__any__")

  var kernelMethod = false

  class SymbolTable(val methodName: String) {
    val labels = new HashMap[SootUnit, String]()
    val params = new HashMap[Int, (Id, Tree)]()
    val mparams = new HashMap[Int, (String, Tree)]()
    val locals = new HashMap[Id, Tree]()
    val arrays = new HashMap[Id, (Tree /*type*/ , Tree /*size*/ )]()
    var thisParam: (Id, Tree) = null
    var kernelMethod = false
    var level: Int = 0

    def addThisParam(typ: SootType, id: Id) = {
      val typUnion = translateType(typ) match {
        case PtrType(v: ValueType) => PtrType(ValueType(v.name + "_intr"))
        case ValueType(name: String) => ValueType(name + "_intr")
        case x => x
      }
      thisParam = (id, typUnion)
    }

    def addParamVar(typ: SootType, index: Int, id: Id) = {
      translateType(typ) match {
        /*
        case PtrType(StructType(n)) if n.equals("firepile_Group") => { }
        case PtrType(StructType(n)) if n.equals("firepile_Item") => { }
        */
        case _ => params += index -> (id, translateType(typ))
      }
    }

    def addParamVar(typ: String, index: Int, name: String) = {
      typ match {
        /*
        case "firepile_Group" => { }
        case "firepile_Item" => { }
        */
        case _ => params += index -> (Id(name), ValueType(typ))
      }
    }

    def addLocalVar(typ: SootType, id: Id) = {
      assert(!params.contains(id))
      // assert(!locals.contains(id))
      /*
      translateType(typ) match {
        case PtrType(StructType(n)) if n.equals("firepile_Group") => { }
        case PtrType(StructType(n)) if n.equals("firepile_Item") => { }
        case _ => locals += id -> translateType(typ)
      }
      */
      locals += id -> translateType(typ)
    }

    def addLocalVar(typ: Tree, id: Id) = {
      assert(!params.contains(id))
      // assert(!locals.contains(id))
      /*
      typ match {
        case PtrType(StructType(n)) if n.equals("firepile_Group") => { }
        case PtrType(StructType(n)) if n.equals("firepile_Item") => { }
        case _ => locals += id -> typ
      }
      */
      locals += id -> typ
    }

    def addArrayDef(typ: SootType, id: Id, size: Tree) = {
      assert(!locals.contains(id))
      println("------ADD ARRAY DEF")
      arrays += id -> (translateType(typ), size)
    }

    def addInlineParams(ip: List[Tree]) = for (p <- ip) { params(params.size) = (Id(p.asInstanceOf[Formal].name + "_c"), p.asInstanceOf[Formal].typ) }
    def addInlineParamsNoRename(ip: List[Tree]) = for (p <- ip) { params(params.size) = (Id(p.asInstanceOf[Formal].name), p.asInstanceOf[Formal].typ) }

    def lastParamIndex = params.size - 1

    var next = -1

    def nextLabel = {
      next += 1
      "lbl_" + next
    }
  }

  private class ClassTable {
    val knownClasses = new HashMap[SootClass, (Tree /* struct */ , Tree /* union */ )]()
    val enumElements = new ListBuffer[Id]()

    def addClass(cls: SootClass) = {
      // HACK to ignore Java classes for now
      if (!knownClasses.contains(cls) && !cls.getName.startsWith("java.")) {
        enumElements += Id(cls.getName + "_ID")

        val sSig = if (cls.getName.contains("$")) getScalaJavaSignature(cls.getName, cls)
        else getScalaSignature(cls.getName)

        val scalaSig = sSig.asInstanceOf[List[ScalaClassDef]]

        if (scalaSig == null)
          throw new RuntimeException("ClassTable::addClass unable to getScalaSignature for " + cls.getName)

        try {
          val superTypeStructs = getSupertypes(scalaSig).filter(st => st match {
            case NamedTyp(name: String) if name.equals("scala.ScalaObject") => false
            case NamedTyp(name: String) if name.equals("java.lang.Object") => false
            case _ => true
          }).map(st => st match {
            case NamedTyp(n: String) => VarDef(StructType(n), Id("_" + n))
            case InstTyp(base: NamedTyp, _) => VarDef(StructType(base.name), Id("_" + base.name))
            case _ => VarDef(Id("UNKNOWN"), Id("_UNKNOWN"))
          })

          val struct = StructDef(Id(cls.getName), VarDef(IntType, Id("__id")) :: superTypeStructs ::: scalaSig.head.fields.map(f => VarDef(translateType(f), f.name)))
          // maybe we should also call addClass on list returned from getDirectSubclassesOf(cls)
          val union = UnionDef(Id(cls.getName + "_intr"), VarDef(StructType("Object"), Id("object")) :: Scene.v.getActiveHierarchy.getSubclassesOfIncluding(cls).map(sc => VarDef(StructType(sc.getName), Id("_" + sc.getName))).toList)

          knownClasses += cls -> (struct, union)
        } catch {
          case e: ClassNotFoundException => {
            println("Class not found: " + e.getMessage)
            knownClasses
          }
        }
      }
    }

    def dumpClassTable = {
      val classtable = List[Tree](StructDef(Id("Object"), VarDef(IntType, Id("__id"))),
        EnumDef(Id("KNOWN_CLASSES"), enumElements.toList)) ::: knownClasses.values.map(v => TreeSeq(v._1, v._2)).toList
      // println("CLASSTABLE CL:")
      // classtable.foreach((ct: Tree) => println(ct.toCL))
      classtable
    }

    def clearClassTable = { knownClasses.clear; enumElements.clear }
  }

  class ArrayStructs {
    val structs = new HashMap[Tree /* type */ , List[Tree] /* struct rep */ ]()

    def addStruct(typ: Tree): Tree = {
      val arrayTyp = typ match {
        case v: ValueType => v.name
        case v: StructType => v.name
        case PtrType(v: ValueType) => v.name
        case _ => throw new RuntimeException("Unknown array type: " + typ)
      }
      if (!structs.contains(typ)) {
        structs += typ -> List(StructDef("g_" + mangleName(arrayTyp).replaceAll(" ", "_") + "Array", List(VarDef(IntType, Id("length")), VarDef(MemType("global", PtrType(typ)), Id("data")))),
          StructDef("l_" + mangleName(arrayTyp).replaceAll(" ", "_") + "Array", List(VarDef(IntType, Id("length")), VarDef(MemType("local", PtrType(typ)), Id("data")))),
          StructDef("c_" + mangleName(arrayTyp).replaceAll(" ", "_") + "Array", List(VarDef(IntType, Id("length")), VarDef(MemType("constant", PtrType(typ)), Id("data")))),
          StructDef("p_" + mangleName(arrayTyp).replaceAll(" ", "_") + "Array", List(VarDef(IntType, Id("length")), VarDef(MemType("private", PtrType(typ)), Id("data")))))
      }

      StructType(arrayTyp.replaceAll(" ", "_") + "Array")
    }

    def dumpArrayStructs = {
      // println("ARRAY STRUCTS CL:")
      // structs.values.flatten.foreach((cl: Tree) => println(cl.toCL))
      structs.values.toList.flatten
    }

    def clearArrays = structs.clear
  }

  class EnvStructs {
    val structs = new HashMap[Tree /* type */ , List[Tree] /* struct rep */ ]()

    def addStruct(typ: Tree): Tree = {
      val arrayTyp = typ match {
        case v: ValueType => v
        case PtrType(v: ValueType) => v
        case _ => throw new RuntimeException("Unknown array type: " + typ)
      }
      if (!structs.contains(typ)) {
      /*
        structs += typ -> List(StructDef("g_" + arrayTyp.name.replaceAll(" ", "_") + "Array", List(VarDef(IntType, Id("length")), VarDef(MemType("global", PtrType(arrayTyp)), Id("data")))),
          StructDef("l_" + arrayTyp.name.replaceAll(" ", "_") + "Array", List(VarDef(IntType, Id("length")), VarDef(MemType("local", PtrType(arrayTyp)), Id("data")))),
          StructDef("c_" + arrayTyp.name.replaceAll(" ", "_") + "Array", List(VarDef(IntType, Id("length")), VarDef(MemType("constant", PtrType(arrayTyp)), Id("data")))),
          StructDef("p_" + arrayTyp.name.replaceAll(" ", "_") + "Array", List(VarDef(IntType, Id("length")), VarDef(MemType("private", PtrType(arrayTyp)), Id("data")))))
      }
      */

      structs += typ -> List(StructDef(arrayTyp.name.replaceAll(" ", "_") + "_ENV", List[Tree]()))
      }
      StructType(arrayTyp.name.replaceAll(" ", "_") + "_ENV")
    }

    def append(structTyp: Tree, varr: VarDef) = {
      structs(structTyp).head match {
        case StructDef(name, vars) => 
          if (!vars.contains(varr))
            structs(structTyp) = List(StructDef(name, varr :: vars))
        case _ => throw new RuntimeException("Existing environment struct not found")
      }
    }

    def contains(structTyp: Tree, name: String) = {
      structs(structTyp).head match {
        case StructDef(sName, vars) => vars.exists(v => { v.asInstanceOf[VarDef].name.equals(mangleName(name)) })
        case _ => throw new RuntimeException("Existing environment struct not found")
      }
    }

    def dumpEnvStructs = {
      // println("ARRAY STRUCTS CL:")
      // structs.values.flatten.foreach((cl: Tree) => println(cl.toCL))
      structs.values.toList.flatten
    }

    def clearEnvs = structs.clear
  }

  // private var symtab: SymbolTable = null

  private val classtab = new ClassTable()

  private val arraystructs = new ArrayStructs()

  private val envstructs = new EnvStructs()

  private def translateLabel(u: SootUnit, symtab: SymbolTable): String = u match {
    case target: Stmt => {
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
    case t: SootVoidType => ValueType("void")
    case t: SootBooleanType => ValueType("int")
    case t: SootByteType => ValueType("char")
    case t: SootShortType => ValueType("short")
    case t: SootCharType => ValueType("ushort")
    case t: SootIntType => ValueType("int")
    case t: SootLongType => ValueType("long")
    case t: SootFloatType => ValueType("float")
    case t: SootDoubleType => ValueType("double")
    case t: SootRefType => {
      t.getSootClass.getName match {
        case "scala.Tuple2" => StructType("Tuple2")
        case "scala.Tuple3" => StructType("Tuple3")
        case "scala.Tuple4" => StructType("Tuple4")
        case "scala.Tuple5" => StructType("Tuple5")
        case "scala.Tuple6" => StructType("Tuple6")
        case "firepile.Spaces$Point1" => IntType
        case "firepile.Spaces$Id1" => StructType(mangleName(t.toString))
        case "firepile.util.Unsigned$UInt" => ValueType("unsigned int")
        case _ => PtrType(StructType(mangleName(t.toString)))
      }
    }
    case t: SootArrayType => { arraystructs.addStruct(translateType(t.getArrayElementType)) }

    //case t: SootArrayType => PtrType(translateType(t.getArrayElementType))
    case t: SootNullType => PtrType(ValueType("void"))
    // TODO: array types
    case _ => ValueType(t.toString)
  }

/*
  private def translateType(memType: String, typ: String, name: String): (Tree, String) = typ match {

    case "java.lang.String" => (MemType(memType, ValueType("char")), "*" + mangleName(name))
    case "firepile.util.Unsigned.UInt" => (ValueType("unsigned int"), mangleName(name))
    case "int" => (ValueType("int"), mangleName(name))
    case "float" => (ValueType("float"), mangleName(name))
    case "long" => (ValueType("long"), mangleName(name))
    case "double" => (MemType(memType, ValueType("double")), mangleName(name))
    case "float[]" => (MemType(memType, ValueType("float")), "*" + mangleName(name))
    case "int[]" => (MemType(memType, ValueType("int")), "*" + mangleName(name))
    case "long[]" => (MemType(memType, ValueType("long")), "*" + mangleName(name))
    case "double[]" => (MemType(memType, ValueType("double")), "*" + mangleName(name))

    case x => (MemType(memType, ValueType(typ)), mangleName(name))
  }
*/

  private def translateType(memType: String, typ: SootType, name: String): (Tree, String) = translateType(typ) match {
      case StructType(n) if n.endsWith("Array") => memType match {
        case "local" => (StructType("l_" + n), mangleName(name))
        case "global" => (StructType("g_" + n), mangleName(name))
      }
      case _ => (MemType(memType, translateType(typ)), mangleName(name))
  }

  private def translateType(t: ScalaVarDef): Tree = t.fieldScalaType match {
    case NamedTyp(s: String) => s match {
      case "scala.Unit" => PtrType(ValueType("void"))
      case "scala.Boolean" => ValueType("BOOL")
      case "scala.Byte" => ValueType("char")
      case "scala.Char" => ValueType("char")
      case "scala.Short" => ValueType("short")
      case "scala.Int" => ValueType("int")
      case "scala.Long" => ValueType("long")
      case "scala.Float" => ValueType("float")
      case "scala.Double" => ValueType("double")
      case "java.lang.String" => PtrType(ValueType("char"))
      case "firepile.util.Unsigned.UInt" => ValueType("unsigned int")
      case "firepile.Spaces$Id1" => StructType(mangleName(t.toString))
      case "int" => ValueType("int")
      case "float[]" => PtrType(ValueType("float"))
      case "int[]" => PtrType(ValueType("int"))
      case "long[]" => PtrType(ValueType("long"))
      case "double[]" => PtrType(ValueType("double"))

      case x => PtrType(ValueType(x))
    }
    case _ => PtrType(ValueType(t.fieldTypeAsString))
  }

  object ScalaMathCall {
    def unapply(v: Value): Option[(String, List[Value])] = {
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
    def unapply(v: Value): Option[(String, List[Value])] = {
      v match {
        // firepile.util.Math.sin(x)
        case GVirtualInvoke(GStaticFieldRef(SFieldRef(SClassName("firepile.util.Math$"), "MODULE$", _, _)), SMethodRef(SClassName("firepile.util.Math$"), name, _, _, _), args) => Some((name, args))
        case _ => None
      }
    }
  }

  object FloatMathCall {
    def unapply(v: Value): Option[(String, List[Value])] = v match {
      // (float) sin((double) x) --> sin(x)
      case GCast(DoubleMathCall(name, args), f) if f.equals(SootFloatType.v) => Some((name, args))
      case _ => None
    }
  }

  object DoubleMathCall {
    def unapply(v: Value): Option[(String, List[Value])] =
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
      case GVirtualInvoke(base, SMethodRef(k@SClassName("scala.Tuple2"), "_1", _, _, _), Nil) => Some(Select(base, "_1"))
      case GVirtualInvoke(base, SMethodRef(k@SClassName("scala.Tuple2"), "_2", _, _, _), Nil) => Some(Select(base, "_2"))
      case GVirtualInvoke(base, SMethodRef(k@SClassName("scala.Tuple3"), "_1", _, _, _), Nil) => Some(Select(base, "_1"))
      case GVirtualInvoke(base, SMethodRef(k@SClassName("scala.Tuple3"), "_2", _, _, _), Nil) => Some(Select(base, "_2"))
      case GVirtualInvoke(base, SMethodRef(k@SClassName("scala.Tuple3"), "_3", _, _, _), Nil) => Some(Select(base, "_3"))
      case _ => None
    }
  }
  /*
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
*/
  // Split library calls into separate objects.
  // This avoids an OutOfMemory error in scalac.
  object LibraryCall {
    def unapply(v: Value)(implicit iv: (SymbolTable, HashMap[String, Value])) = {
      val (symtab, anonFuns) = iv
      val t: Tree = v match {
        // These are just here as examples for matching Array.ofDim.  We need a realy strategy for translating newarray.
        
        case GVirtualInvoke(GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)), SMethodRef(_, "ofDim", _, _, _),
          List(size, GVirtualInvoke(GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)), SMethodRef(_, "Int", _, _, _), Nil))) =>
          Call(Id("newIntArray"), List[Tree](size))

        case GVirtualInvoke(GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)), SMethodRef(_, "ofDim", _, _, _),
          List(size, GVirtualInvoke(GStaticFieldRef(SFieldRef(_, "MODULE$", _, _)), SMethodRef(_, "Float", _, _, _), Nil))) =>
          Call(Id("newFloatArray"), List[Tree](size))
        
        /* case UnboxCall(t) => t */
        case TupleSelect(t) => t

        // firepile.util.Math.sin(x)
        case FirepileMathCall(name, args) => Call(Id(name), args.map(a => translateExp(a, symtab, anonFuns)))

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
        case ScalaMathCall(name, args) => Call(Id(name), args.map(a => translateExp(a, null, null)))

        case _ => null
      }
      if (t == null) None else Some(t)
    }
  }

  object IntTyped {
    def unapply(v: AnyRef) = v match {
      case x: Value if SootIntType.v.equals(x.getType) => Some(x)
      case x: ValueBox if SootIntType.v.equals(x.getValue.getType) => Some(x.getValue)
      case _ => None
    }
  }

  object LongTyped {
    def unapply(v: AnyRef) = v match {
      case x: Value if SootLongType.v.equals(x.getType) => Some(x)
      case x: ValueBox if SootLongType.v.equals(x.getValue.getType) => Some(x.getValue)
      case _ => None
    }
  }

  object DoubleTyped {
    def unapply(v: AnyRef) = v match {
      case x: Value if SootDoubleType.v.equals(x.getType) => Some(x)
      case x: ValueBox if SootDoubleType.v.equals(x.getValue.getType) => Some(x.getValue)
      case _ => None
    }
  }

  object FloatTyped {
    def unapply(v: AnyRef) = v match {
      case x: Value if SootFloatType.v.equals(x.getType) => Some(x)
      case x: ValueBox if SootFloatType.v.equals(x.getValue.getType) => Some(x.getValue)
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
    else if (k.getName.equals("scala.Function1"))
      true
    else if (k.getName.equals("scala.Function2"))
      true
    else if (k.getName.equals("scala.Function3"))
      true
    else if (k.hasSuperclass && isFunctionClass(k.getSuperclass))
      true
    else
      false
  }

  private def isFunction(t: SootType): Boolean = t match {
    case t: SootRefType => t.hasSootClass && isFunctionClass(t.getSootClass)
    case _ => false
  }

  object Operator {
    def unapply(v: Value)(implicit iv: (SymbolTable, HashMap[String, Value]) = null) = {
      val result = v match {
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
        case _ => null
      }

      if (result != null)
        Some(result)
      else
        None
    }
  }

  private def translateExp(v: Value, symtab: SymbolTable, anonFuns: HashMap[String, Value]): Tree = {
    implicit val iv: (SymbolTable, HashMap[String, Value]) = (symtab, anonFuns)
    v match {

      case _ => {

        println("::v::" + v)
        v match {
          // Must be first
          case LibraryCall(t) => t

          // Must be second
          case Operator(t) => t

          case GNullConstant() => Id("NULL")

          case GIntConstant(value) => IntLit(value)
          case GLongConstant(value) => LongLit(value)
          case GFloatConstant(value) => FloatLit(value)
          case GDoubleConstant(value) => DoubleLit(value)
          case GUIntConstant(value) => UIntLit(value)
          case GArrayLength(op) => Select(op, "length")

          case GCast(op, castTyp) => Cast(translateType(castTyp), translateExp(op, symtab, anonFuns))

          // IGNORE
          case GInstanceof(op, instTyp) => Id("unimplemented:instanceof")

          // IGNORE
          case GNew(newTyp) => { /* classtab.addClass(new SootClass(newTyp.asInstanceOf[SootType].toString));*/ Id("unimplemented:new") }

          // IGNORE
          case GNewArray(newTyp, size) => Id("unimplemented:newarray")
          // IGNORE
          case GNewMultiArray(newTyp, sizes) => Id("unimplemented:newmultiarray")

          case GNewInvoke(baseTyp, method@SMethodRef(_, "<init>", _, _, _), args) => {

            //println("baseName:::" + baseTyp.getSootClass.getName)
            //for (i <- args) println(" args:" + i)

            if (baseTyp.getSootClass.getName.equals("scala.Tuple2")) {
              Cast(StructType("Tuple2"), StructLit(args.map {
                // scala.runtime.BoxesRunTime.boxToFloat(float) : Object
                case v@GStaticInvoke(SMethodRef(k@SClassName("scala.runtime.BoxesRunTime"), "boxToInt", _, _, _), List(value)) => {
                  val name = freshName("union")
                  symtab.addLocalVar(ANY_TYPE, Id(name))
                  Comma(Assign(Select(Id(name), "i"), translateExp(value, symtab, anonFuns)), Id(name))
                }
                case v@GStaticInvoke(SMethodRef(k@SClassName("scala.runtime.BoxesRunTime"), "boxToFloat", _, _, _), List(value)) => {
                  val name = freshName("union")
                  symtab.addLocalVar(ANY_TYPE, Id(name))
                  Comma(Assign(Select(Id(name), "f"), translateExp(value, symtab, anonFuns)), Id(name))
                }
                case v => translateExp(v, symtab, anonFuns)
              }))
            } else if (isFunction(baseTyp)) {
              // worklist += CompileMethodTask(method, baseTyp)
              /*
          println("CLOSURE METHOD: " + method.name)
          // TODO:  Need to translate methods with java.lang.Object parameters also, can't always just filter them out
          val applyMethods = baseTyp.getSootClass.getMethods.filter(mn => mn.getName.equals("apply") && !mn.getParameterType(0).toString.equals("java.lang.Object"))

          for (applyMethod <- applyMethods) {
            println("Found apply method: " + applyMethod.getSignature)
            worklist += CompileMethodTask(applyMethod, baseTyp)
          }

          arraystructs.structs += ValueType("EnvX") -> List(StructDef(Id("EnvX"), args.map(ca => VarDef(translateType(ca.getType), Id(mangleName(ca.asInstanceOf[Local].getName))))))

          TreeSeq(VarDef(StructType("EnvX"), Id("this")) :: args.map(ca => Assign(Select(Id("env"), Id(mangleName(ca.asInstanceOf[Local].getName))), Id(mangleName(ca.asInstanceOf[Local].getName)))) :::
          List(ClosureCall(Id(mangleName(baseTyp.toString) + method.name), args.map(ca => translateExp(ca, symtab, anonFuns)))))
*/

              // TODO: add #define makeClosure(s) NULL [or similar] to preamble
              Call(Id("makeClosure"), args.map(a => translateExp(a, symtab, anonFuns)))
            } else {
              // worklist += CompileMethodTask(method)
              // throw new RuntimeException("Cannot create new instances of classes")

              // TODO: Some things call new such as java.lang.Float.valueOf, need a way to handle this
              Call(Id("_init_"), Call(Id("new_" + mangleName(baseTyp.toString)), args.map(a => translateExp(a, symtab, anonFuns))))
            }
          }

          //case GInstanceFieldRef(instBase, fieldRef) => { println(" GInstanceFieldRef::"+instBase+"::"+fieldRef); Id(fieldRef.name) }
          case GStaticInvoke(method@SMethodRef(_, "boxToFloat", _, _, _), args) => args.map(a => translateExp(a, symtab, anonFuns)).head
          case GStaticInvoke(method@SMethodRef(_, "unboxToFloat", _, _, _), args) => args.map(a => translateExp(a, symtab, anonFuns)).head
          case GStaticInvoke(method, args) => {
            println(" static method:" + method)
            println(" static method NAME:" + method.name)
            println("methodName(method):" + methodName(method))
            for (i <- args) println("arg::" + i)

            worklist += CompileMethodTask(method)
            // classtab.addClass(method.declaringClass)
            Call(Id(method.name), args.map(a => translateExp(a, symtab, anonFuns)))
          }
          case GSpecialInvoke(base: Local, method, args) => {
            worklist += CompileMethodTask(method, true)
            //Call(Select(base, method.name), Id("_this") :: args.map(a => translateExp(a)))
            // classtab.addClass(method.declaringClass)
            Call(Id(methodName(method)), Id("_this") :: args.map(a => translateExp(a, symtab, anonFuns)))
          }

          // Ignore Point conversion to int for now
          case GVirtualInvoke(_, method@SMethodRef(_, "point12int", _, _, _), args) => args.map(a => translateExp(a, symtab, anonFuns)).head

          case GVirtualInvoke(base, method, args) => {
            handleIdsVirtualInvoke(v, symtab, anonFuns) match {
              case Some(x) => x
              // case _ => defaultGVirtualInvoke(base, method, args, symtab, anonFuns)
              case _ => {
                //implicit val iv: (SymbolTable, HashMap[String,Value]) = (symtab, anonFuns)
                val anonFunParams = new ListBuffer[(Int, Value)]()
                var argCount = 0
                args.zipWithIndex.foreach {
                  case (a, argCount) => if (isFunction(a.getType)) { /* println("ADDING ANONFUN AS PARAM: " + a); */ anonFunParams += ((argCount, a)) }
                }

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
                //       case TYPE_B: B_m((B*) x)R
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

                val possibleReceivers = getPossibleReceivers(base, method)
                // println("possibleReceiver(" + base + ", " + method + ") = " + possibleReceivers)

                assert(possibleReceivers.length > 0)

                val methodSig = method.name + soot.AbstractJasminClass.jasminDescriptorOf(method)

                val methodReceiversRef = ListBuffer[SootMethod]()

                for (pr <- possibleReceivers) {
                  for (m <- pr.methodIterator) {
                    if (!m.isAbstract) {
                      val sig = m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef)
                      // println("Checking method: " + sig + " against " + methodSig)
                      if (sig.equals(methodSig)) {
                        // println("Adding CompileMethodTask(" + method + ", " + pr + ")")
                        if (args.length > 0)
                          worklist += CompileMethodTask(method, true, anonFunParams.toList)
                        else
                          worklist += CompileMethodTask(method, true)
                        methodReceiversRef += m
                        // classtab.addClass(pr)
                      }
                    }
                  }
                }

                val anonFunsLookup = new HashMap[String, Value]()

                for (af <- anonFunParams) {
                  val (index, value) = af
                  val declaringClass = method.resolve.getDeclaringClass
                  // println("FUNCTION TYPE: " + value.getType.toString + " IS ARG OF " + declaringClass)
                  val classToPass = if (declaringClass.toString.contains("\\$"))
                    new SootClass(declaringClass.toString)
                  else
                    declaringClass
                  getSignature(declaringClass.toString, classToPass) match {
                    case (cdef: ScalaClassDef) :: _ =>
                      cdef.methods.filter(m => m.name.equals(method.name)).headOption match {
                        case Some(mdef) =>
                          val paramName = mdef.params(index).name
                          // println("FUNCTION PARAM IS NAMED: " + paramName)

                          anonFunsLookup += paramName -> value
                        // TODO
                        case None => {
                          // printClassDef(List(cdef))
                          throw new RuntimeException("method " + method.name + " not found in class " + cdef.name + " as expected; search the supertype(s)!")
                        }
                      }
                    /*
                case (cdef: ScalaJavaClassDef) :: _ =>
                  cdef.methods.filter(m => m.name.equals(method.name)).headOption match {
                    case Some(mdef) => 
                      val paramName = mdef.params(index).name
                      println("FUNCTION PARAM IS NAMED: " + paramName)

                      anonFunsLookup += paramName -> value
                    // TODO
                    case None => throw new RuntimeException("method " + method.name + " not found in class " + cdef.name + " as expected; search the supertype(s)!")
                  }
                case (cdef: JavaClassDef) :: _ =>
                  cdef.methods.filter(m => m.name.equals(method.name)).headOption match {
                    case Some(mdef) => 
                      val paramName = mdef.params(index).name
                      println("FUNCTION PARAM IS NAMED: " + paramName)

                      anonFunsLookup += paramName -> value
                    // TODO
                    case None => throw new RuntimeException("method " + method.name + " not found in class " + cdef.name + " as expected; search the supertype(s)!")
                  }
                  */

                    case x => { println("getSignature is returning: " + x); throw new RuntimeException("scala sig for " + declaringClass + " not found -- should not happen") }
                  }
                }

                val FunDef(_, _, addParams, _) = compileMethod(method.resolve, symtab.level + 1, false, anonFunsLookup)

                assert(possibleReceivers.length == methodReceiversRef.length)

                val argsToPass = args.map(a => translateExp(a, symtab, anonFuns)) // Will need to cast "self" to appropriate type

                if (possibleReceivers.length == 1) {
                  // monomorphic call
                  // should be: Call(Id(methodName(method)), translateExp(base)::args.map(a => translateExp(a)))
                  // println("Monomorphic call to " + methodName(method))

                  symtab.addInlineParamsNoRename(addParams.takeRight(addParams.length - method.resolve.getParameterCount).filter(p => {
                    p match {
                      case Formal(PtrType(StructType(s)), _) if s.startsWith("firepile_Spaces_Id") => false
                      case _ => true
                    }
                  }).map(p => p match {
                    case Formal(StructType(s), name) if s.endsWith("Array") => Formal(StructType("l" + s.substring(1)), name)
                    case x => x
                  }))

                  // Find Id param, remove it and create/populate Id struct
                  val idStructPops = ListBuffer[Tree]()

                  idStructPops += Call(Id(methodName(methodReceiversRef.head)), /* Id("_this") :: */ argsToPass ::: addParams.takeRight(addParams.length - method.resolve.getParameterCount).map(p => p match {
                    case Formal(StructType(s), name) if s.startsWith("firepile_Spaces_Id") => Id(name)
                    case Formal(StructType(s), name) => Id(name)
                    case _ => Id(p.asInstanceOf[Formal].name)
                  }))

                  // println("RETURNING idStructPops: " + TreeSeq(idStructPops.toList))
                  TreeSeq(idStructPops.toList)

                  // TODO: pass in base, not this.  See 'should be' above :-)

                  // If base is a 'new anonfun', should generate the call to apply right here (above).
                  // calls on 'new anonfun' should not be polymorphic.
                } else {
                  // polymorphic call--generate a switch

                  val methodFormals: List[Tree] = compileMethod(method.resolve, 0, false, null) match {
                    case FunDef(_, _, formals, _) => formals
                    case _ => Nil
                  }

                  val methodFormalIds: List[Id] = methodFormals.map(mf => mf match {
                    case Formal(_, name) => Id(name)
                    case _ => Id("Wtf: No Name?")
                  })

                  val methodReceivers = methodReceiversRef.map(mr => methodName(mr))

                  val switchStmt = Switch(Id("cls->object.__id"), (possibleReceivers zip methodReceivers).map(mr => Case(Id(mr._1.getName + "_ID"), TreeSeq(Return(Call(Id(mr._2), (Cast(PtrType(Id(mr._1.getName + "_intr")), Id("cls")) :: methodFormalIds))))))) // really no need for a break if we are returning

                  // add appropriate formal parameters for call
                  worklist += new CompileMethodTree(FunDef(ValueType(method.returnType.toString), Id("dispatch_" + method.declaringClass.getName), Formal(PtrType(Id(method.declaringClass.getName + "_intr")), "cls") :: methodFormals, List(switchStmt).toArray: _*))

                  // FunDef(translateType(m.getReturnType), Id(methodName(m)), paramTree.toList, (varTree.toList ::: result).toArray:_*)
                  // Call(Id("unimplemented: call to " + methodName(method)), TreeSeq())
                  Call(Id("dispatch_" + method.declaringClass.getName), Id(base.asInstanceOf[Local].getName) :: argsToPass)
                }

              }
            }
          }

          //g$1.<firepile.Group: scala.collection.immutable.List items()>()::<: int ()>::List()

          case GInterfaceInvoke(base: GVirtualInvokeExpr, SMethodRef(SClassName("scala.collection.SeqLike"), "size", _, _, _), _) => { 
              println(" Got Local Size::"); 
              // Use real base name of firepile_Group
              // COMMENTED OUT until these are passed around as args
              /*
              base.getBase match {
                case b: soot.grimp.internal.GInstanceFieldRef => return Select(Id(mangleName(b.getField.getName)), Id("size"))
                case b: Local => return Select(Id(b.getName), Id("size"))
                case _ => throw new RuntimeException("Getting size of some unknown collection")
              }
              */
              Select(Id("_group_desc"), Id("size"))
          }
/*
              return Select(Id(base.getBase.asInstanceOf[soot.grimp.internal.GInstanceFieldRef].toString /*getBase.asInstanceOf[Local].getName*/), Id("size")) }
*/
          case GInterfaceInvoke(base, method, args) => base match {

            case b: Local => {
              worklist += CompileMethodTask(method)
              // need to find all subclasses of method.getDeclaringClass that override method (i.e., have the same _.getSignature)
              // classtab.addClass(new SootClass(b.getName))
              // println("INTERFACE INVOKE: " + methodName(method) + " NAME " + b.getName.toString + " TYPE " + b.getType.toString)

              // also call translateExp(base)

              anonFuns.get(b.getName) match {
                case Some(GNewInvoke(closureTyp, closureMethod, closureArgs)) if b.getType.toString.startsWith("scala.Function") =>
                  // new AnonFun1$blah(closureArgs).method(args)
                  /*ClosureCall("blah_apply", new AnonFun1$blah(closureArgs) :: args)
                or:
                ClosureCall("blah_apply", closureArgs ::: args)
                or:
                */
                  // println("CLOSURE METHOD: " + method.name)
                  // TODO:  Need to translate methods with java.lang.Object parameters also, can't always just filter them out
                  val applyMethods = closureTyp.getSootClass.getMethods.filter(mn => mn.getName.equals(method.name) && !mn.getParameterType(0).toString.equals("java.lang.Object"))

                  var fd: FunDef = null
                  /*
              for (applyMethod <- applyMethods) {
                println("Found apply method: " + applyMethod.getSignature)
              }
              */

                  fd = compileMethod(applyMethods.head, symtab.level + 1, false, anonFuns)

                  symtab.addInlineParams(fd.formals)

                  worklist += CompileMethodTask(applyMethods.head)
                  ClosureCall(Id(mangleName(closureTyp.toString) + method.name), closureArgs.map(ca => translateExp(ca, symtab, anonFuns)) ::: args.map(a => translateExp(a, symtab, anonFuns)))

                // case _ => Call(Select(b, method.name), args.map(a => translateExp(a, symtab, anonFuns)))
                case _ => Call(Id(method.name), args.map(a => translateExp(a, symtab, anonFuns)))
              }

              /*
          if(base.getType.toString.startsWith("scala.Function"))
            ClosureCall(Id(base.getName), args.map(a => translateExp(a, anonFuns)))
          else
            Call(Select(base, method.name), args.map(a => translateExp(a, anonFuns)))
          */
            }
            case GInstanceFieldRef(instBase, fieldRef) => {

              println(" GInstanceFieldRef::" + instBase + "::" + fieldRef)
              // printf("GInstanceFieldRef:: instBase name = " + instBase.asInstanceOf[Local].getName + " with method name " + method.name)
              // TODO: comment
              // Handle accessing captured variables.  Assumes captured variable 'x' is 'this.x$1'
              // TODO: eliminate (if possible) the assumptions here about the naming conventions of variables.
              if (fieldRef.`type`.toString.startsWith("scala.Function")) {
                println("======= LOOKING FOR fieldRef: " + fieldRef.name + " in ")
                anonFuns.keys.foreach(k => print(k + " "))
                anonFuns(fieldRef.name.takeWhile(_ != '$')) match {
                  // matched 'this.x$1.apply(args)' where 'x$1' is a captured variable 'x'
                  // and 'x' is 'new anonfun$1(closureArgs)'
                  // ->
                  // anonfun_dollar1_apply(env, args)
                  // and also translate the body of that method
                  case GNewInvoke(closureTyp, closureMethod, closureArgs) => {
                    // println("CLOSURE METHOD: " + fieldRef.name)
                    // TODO:  Need to translate methods with java.lang.Object parameters also, can't always just filter them out
                    val applyMethods = closureTyp.getSootClass.getMethods.filter(mn => mn.getName.equals(method.name) && !mn.getParameterType(0).toString.equals("java.lang.Object"))

                    /*
                for (applyMethod <- applyMethods) {
                  println("Found apply method: " + applyMethod.getSignature)
                }
                */

                    worklist += CompileMethodTask(applyMethods.head)
                    ClosureCall(Id(mangleName(closureTyp.toString + method.name)), args.map(a => translateExp(a, symtab, anonFuns)))
                  }
                  //case _ => Select(base, mangleName(fieldRef.name)) // TODO: punt
                  case _ => println(" mangled name::" + mangleName(fieldRef.name)); Id(mangleName(fieldRef.name))
                }
                //} else Select(base, mangleName(fieldRef.name)) // TODO: punt
              } else { println(" mangled name::" + mangleName(fieldRef.name)); Id(mangleName(fieldRef.name)) }

            }

            case GStaticInvoke(method, args) => Id("STATICINVOKE inside INTERFACE INVOKE")

            //Changes here
            case GStaticFieldRef(_) => Id("static field ref")
            case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Space"), "groups", _, _, _), _) => {
                var applyM: SootMethod = null
                for (a <- args) {
                  println("Getting SootClass for: " + a.getType.toString)
                  val sootcls = Scene.v.getSootClass(a.getType.toString)
                  val applyMethods = sootcls.getMethods.filter(mn => mn.getName.equals("apply")  && !mn.getParameterType(0).toString.equals("java.lang.Object"))
                  println("number of methods = " + sootcls.getMethodCount)

                  println("Apply methods found: " + applyMethods.length)
                  for (applyMethod <- applyMethods) {
                    println("Found method: " + applyMethod.getSignature)
                    worklist += CompileMethodTask(applyMethod.makeRef)
                    compileMethod(applyMethod, 0, false, anonFuns)
                    applyM = applyMethod
                  }

                }
                Call(Id(methodName(applyM)), Id("NULL"))
            }
            case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Group"), "items", _, _, _), _) => {
                var applyM: SootMethod = null
                for (a <- args) {
                  println("Getting SootClass for: " + a.getType.toString)
                  val sootcls = Scene.v.getSootClass(a.getType.toString)
                  val applyMethods = sootcls.getMethods.filter(mn => mn.getName.equals("apply")  && !mn.getParameterType(0).toString.equals("java.lang.Object"))
                  println("number of methods = " + sootcls.getMethodCount)

                  println("Apply methods found: " + applyMethods.length)
                  for (applyMethod <- applyMethods) {
                    println("Found method: " + applyMethod.getSignature)
                    worklist += CompileMethodTask(applyMethod.makeRef)
                    applyM = applyMethod
                  }

                }
                Call(Id(methodName(applyM)), Id("NULL"))
            }
            case x => Id("unsupported interface invoke:" + v.getClass.getName + " :::::: " + v + " --> " + x)
          }

          case GLocal(name, typ) => v match {

            case GLocal(baseName, SMethodRef(classname, "Something", _, _, _)) => {
              if (anonFuns.contains(name))
                translateExp(anonFuns(name), symtab, anonFuns)
              else
                symtab.addLocalVar(typ, Id(mangleName(name))); Id(mangleName(name))
            }
            case _ => {
              if (anonFuns.contains(name))
                translateExp(anonFuns(name), symtab, anonFuns)
              else
                symtab.addLocalVar(typ, Id(mangleName(name))); Id(mangleName(name))
            }
          }
          case GThisRef(typ) => { symtab.addThisParam(typ, Id("_this")); Id("_this") }
          //case GThisRef(typ) => { Id("_this") }
          case GParameterRef(typ, index) => { 
            if (!symtab.kernelMethod) { symtab.addParamVar(typ, index, Id("_arg" + index)); Id("_arg" + index) }
            else Id("_this")
          }
          // case GParameterRef(typ, index) => { Id("_this") }
          case GStaticFieldRef(fieldRef) => { /* classtab.addClass(new SootClass(fieldRef.`type`.toString));*/ Id("unimplemented:staticfield") }

          case GInstanceFieldRef(base: Local, fieldRef) => { /* classtab.addClass(new SootClass(base.getName)); */
            //Select(Deref(base), mangleName(fieldRef.name))
            println(" mangled Name::" + mangleName(fieldRef.name) + "  original::" + fieldRef.name)
            // Look up instance in environment
            if (envstructs.contains(ValueType("kernel"), mangleName(fieldRef.name))) {
              Select(Id("_this_kernel"), Id(mangleName(fieldRef.name)))
            }
            else
              Id(mangleName(fieldRef.name))
          }

          case GInstanceFieldRef(base, fieldRef) => { 
            println(" base ::" + base + ":::" + fieldRef)

            // Look up instance in environment
            if (envstructs.contains(ValueType("kernel"), mangleName(fieldRef.name))) {
              Select(Id("_this_kernel"), Id(mangleName(fieldRef.name)))
            }
            else
              Id(mangleName(fieldRef.name))
          }

          case GArrayRef(base, index) => ArrayAccess(Select((translateExp(base, symtab, anonFuns)), "data"), translateExp(index, symtab, anonFuns))

/*
          case GArrayRef(base, index) => {
            //println(" Array Access::"+ base+"::"+index); 

            base match {

              case GInstanceFieldRef(instBase, fieldRef) => { println("Different Inside Array Access::" + instBase + "::" + fieldRef.name); Id(mangleName(fieldRef.name) + "[" + (translateExp(index, symtab, anonFuns)).toCL + "]") }

              case _ => Id(mangleName((translateExp(base, symtab, anonFuns)).toCL) + "[" + (translateExp(index, symtab, anonFuns)).toCL + "]")
              //case _ =>  ArrayAccess(translateExp(base, symtab, anonFuns), index)
            }

          }
*/

          case v => Id("translateExp:unsupported:" + v.getClass.getName)

        }
      }
    }
  }

  private def handleIdsVirtualInvoke(v: Value, symtab: SymbolTable, anonFuns: HashMap[String, Value]): Option[Tree] = {
    v match {
      // TODO:
      // turn these into field accesses on the struct Id1 passed into the kernel
      // don't assume the local is named "id"
      // handle Id2, Id3, ...r1.

      case GVirtualInvoke(_, SMethodRef(SClassName(_), "barrier", _, _, _), _) => { println(" Got barrier here::"); return Some(Call(Id("barrier"), Id("CLK_LOCAL_MEM_FENCE"))) }
      case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Item"), "id", _, _, _), args) => { println(" Got Item here::"); if (args.size > 0) return Some(Call(Id("get_local_id"), Id(translateExp(args.head, symtab, anonFuns).toCL))) else return Some(Select(Id("_item_desc"), Id("id"))) }
      case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Group"), "id", _, _, _), args) => { println(" Got Group here::"); if (args.size > 0) return Some(Call(Id("get_group_id"), Id(translateExp(args.head, symtab, anonFuns).toCL))) else return Some(Select(Id("_group_desc"), Id("id"))) }
      case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Group"), "size", _, _, _), args) => { println(" Got Global Size here::"); if (args.size > 0) return Some(Call(Id("get_global_size"), Id(translateExp(args.head, symtab, anonFuns).toCL))) else return Some(Select(Id("_group_desc"), Id("size"))) }
      case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Item"), "globalId", _, _, _), args) => { println(" Got Global ID here::"); if (args.size > 0) return Some(Call(Id("get_global_id"), Id(translateExp(args.head, symtab, anonFuns).toCL))) else return Some(Select(Id("_item_desc"), Id("globalId"))) }
      case GVirtualInvoke(base: Local, SMethodRef(SClassName("firepile.Item"), "size", _, _, _), args) => { println(" Got Local size here::"); if (args.size > 0) return Some(Call(Id("get_local_size"), Id(translateExp(args.head, symtab, anonFuns).toCL))) else return Some(Select(Id(base.getName), Id("size"))) }

      case GVirtualInvoke(a, method, args) => { println("Generic Virtual Invoke:::" + a + "::" + method + ":::" + args) }
      case _ => ;
    }

    None
  }
  private def translateUnits(units: List[SootUnit], result: List[Tree], symtab: SymbolTable, anonFuns: HashMap[String, Value]): List[Tree] = {
    implicit val iv: (SymbolTable, HashMap[String, Value]) = (symtab, anonFuns)
    units match {
      case u :: us => {
        val tree: Tree = u match {
          case GIdentity(left: Value, right) => translateType(left.getType) match {
            /*
              case PtrType(StructType(n)) if n.equals("firepile_Group") => TreeSeq()
              case PtrType(StructType(n)) if n.equals("firepile_Item") => TreeSeq()
            */
              case _ => {
                // create kernel environment struct
                envstructs.addStruct(ValueType("kernel"))
                left match {
                  case l: Local if !(l.getName.equals("this") || l.getName.equals("l0"))=> envstructs.append(ValueType("kernel"), VarDef(translateType(left.getType), mangleName(l.getName)))
                  case l: Local => { }
                  case _ => throw new RuntimeException("Some identity type other than local being added to env struct")
                }
                Eval(Assign(left, right))
              }
          }
          case GAssignStmt(left: Local, GNewArray(typ: SootArrayType, size)) => {
            symtab.locals -= Id(left.getName); symtab.addArrayDef(typ.getElementType, Id(left.getName), translateExp(size, symtab, anonFuns)); TreeSeq()
          }
          case GAssignStmt(left: Local, right) => {
              println(" Level 2::" + left.getName)
              right match {

                case GCast(GVirtualInvoke(_, method, args), typ) => {
                  args.head match {
                    case GInterfaceInvoke(GVirtualInvoke(_, SMethodRef(_, "items", _, _, _), args), SMethodRef(_, "size", _, _, _), _) => {
                      println(" Setting local variable::" + left.getName + "::" + typ.toString + "::" + Kernel.blocks)
                      Kernel.localArgs.add((left.getName, typ, Kernel.blocks))
                      val fieldType = translateType(typ) match {
                        case ft: StructType => StructType("l_" + mangleName(ft.name))
                        case x => x
                      }
                      envstructs.append(ValueType("kernel"), VarDef(fieldType, mangleName(left.getName)))
                      TreeSeq()
                    }
                    case _ => TreeSeq()
                  }
                }

                case _ => Eval(Assign(left, right))
              }
          }
          case GAssignStmt(left, right) => Eval(Assign(left, right))

          case GGoto(target) => GoTo(translateLabel(target, symtab))
          case GNop() => Nop
          case GReturnVoid() if Kernel.level == 2 => {
            println(" Changing to Level 3 ")
            Kernel.level = 3;
            // return List(TreeSeq())
            Return
          } 
          case GReturnVoid() => Return
          case GReturn(returned) if Kernel.level == 2 => {
            println(" Changing to Level 3 ")
            Kernel.level = 3;
            // return List(TreeSeq())
            Return
          } 
          case GReturn(returned) => returned match {
            // TODO: Maybe remove this????
            case ni@GNewInvoke(closureTyp, closureMethod, closureArgs) => {
            /*
              if (Kernel.level == 1) {
                println("Changing to Level 2")
                Kernel.level = 2
              }
            */
                for (i <- 0 until closureArgs.length) {
                  closureArgs(i) match {
                    case GInstanceFieldRef(instBase, fieldRef) => { 
                      println(" Global Variable from inst field ref " + instBase + " :::" + fieldRef.name + ":::" + fieldRef.`type`.toString)
                      Kernel.globalArgs.add((fieldRef.name, fieldRef.`type`,i))
                      envstructs.addStruct(ValueType("kernel"))
                      val fieldType = translateType(fieldRef.`type`) match {
                        case ft: StructType => StructType("g_" + mangleName(ft.name))
                        case x => x
                      }
                      envstructs.append(ValueType("kernel"), VarDef(fieldType, mangleName(fieldRef.name)))
                    }
                    case GStaticInvoke(SMethodRef(SClassName(_), name, _, _, _), args) => {
                      for (j <- args) {
                        j match {
                          case GInstanceFieldRef(instBase, fieldRef) => { 
                            println(" Global Variable from static invoke with instance field ref arg " + instBase + " :::" + fieldRef.name + ":::" + fieldRef.`type`.toString)
                            Kernel.globalArgs.add((fieldRef.name, fieldRef.`type`,i))
                            envstructs.addStruct(ValueType("kernel"))
                            val fieldType = translateType(fieldRef.`type`) match {
                              case ft: StructType => StructType("g_" + mangleName(ft.name))
                              case x => x
                            }
                            envstructs.append(ValueType("kernel"), VarDef(fieldType, mangleName(fieldRef.name)))
                          }
                          case _ => {}
                        }
                      }
                    }
                    case _ => {}
                  }
                }
//                return List(TreeSeq())
//              }
             
            println("closureType = " + closureTyp.toString + " closureMethod = " + closureMethod.name)
            Return
              //translateExp(ni, symtab, anonFuns)

/*
              val applyMethods = closureTyp.getSootClass.getMethods.filter(mn => mn.getName.equals("apply"))
 
              for (applyMethod <- applyMethods) {
                println("Found apply method: " + applyMethod.getSignature)
              }


              var fd: FunDef = null
              if (applyMethods.length > 0)
                fd = compileMethod(applyMethods.head, symtab.level + 1, false, anonFuns)
              else
                fd = compileMethod(closureMethod.resolve, symtab.level + 1, false, anonFuns)

              // Add closure formals to calling function params
              symtab.addInlineParams(fd.formals.map(p => p match {
                case Formal(StructType(s), name) if s.endsWith("Array") => Formal(StructType("l" + s.substring(1)), name)
                case x => x
              }))

              // Add closure function to worklist that takes ENV struct
              worklist += CompileMethodTree(FunDef(fd.typ, fd.name, Formal(PtrType(StructType(symtab.methodName + "_EnvX")), Id("this")) :: fd.formals.map(f =>
                f match {
                  case Formal(StructType(s), name) if s.endsWith("Array") => Formal(StructType("l" + s.substring(1)), name)
                  case x => x
                }), fold({
                case StructType(s) if s.endsWith("Array") => StructType("l" + s.substring(1))
                case t => t
              })(fd.body)))

              envstructs.structs += ValueType(symtab.methodName + "_EnvX") -> List(StructDef(Id(symtab.methodName + "_EnvX"), closureArgs.filter(ca => ca.isInstanceOf[Local]).map(ca => VarDef(translateType(ca.getType), Id(mangleName(ca.asInstanceOf[Local].getName))))))

              TreeSeq(VarDef(StructType(symtab.methodName + "_EnvX"), Id("this")) :: closureArgs.filter(ca => ca.isInstanceOf[Local]).map(ca => Assign(Select(Id("this"), Id(mangleName(ca.asInstanceOf[Local].getName))), Id(mangleName(ca.asInstanceOf[Local].getName)))) :::
                List(ClosureCall(Id(mangleName(closureTyp.toString) + "apply"), Ref(Id("this")) :: fd.formals.map(fp => Id(fp.asInstanceOf[Formal].name + "_c")))))
              */
            }

            case GVirtualInvoke(base, method, args) => translateExp(returned, symtab, anonFuns)

            case GInterfaceInvoke(base, method, args) if base.getType.toString.startsWith("scala.Function") => {
              // println("GReturn::NewInvoke Found apply method: " + applyMethod.getSignature)
              val applyMethods = new SootClass(base.getType.toString).getMethods.filter(mn => mn.getName.equals("apply"))

              if (applyMethods.length > 0)
                compileMethod(applyMethods.head, symtab.level + 1, false, anonFuns)
              else
                compileMethod(method.resolve, symtab.level + 1, false, anonFuns)

              envstructs.structs += ValueType("EnvX") -> List(StructDef(Id("EnvX"), args.map(ca => VarDef(translateType(ca.getType), Id(mangleName(ca.asInstanceOf[Local].getName))))))

              TreeSeq(VarDef(StructType("EnvX"), Id("this")) :: args.map(ca => Assign(Select(Id("this"), Id(mangleName(ca.asInstanceOf[Local].getName))), Id(mangleName(ca.asInstanceOf[Local].getName)))) :::
                List(ClosureCall(Id(mangleName(base.toString) + method.name), args.map(ca => translateExp(ca, symtab, anonFuns)))))
            }
            case _ => Return(translateExp(returned, symtab, anonFuns)) match {
              case Return(Cast(typ, StructLit(fields))) => {
                val tmp = freshName("ret")
                symtab.addLocalVar(typ, Id(tmp))
                // println(" typ ::" + typ + ":: fields" + fields + " tmp::" + tmp)
                TreeSeq(Eval(Assign(Id(tmp), Cast(typ, StructLit(fields)))), Return(Id(tmp)))
              }
              case t => t
            }
          }
          case GIf(cond, target) => If(translateExp(cond, symtab, anonFuns), GoTo(translateLabel(target, symtab)), Nop)
          case GInvokeStmt(invokeExpr) => Eval(translateExp(invokeExpr, symtab, anonFuns))

          // TODO
          case GTableSwitchStmt(key, lowIndex, highIndex, targets, defaultTarget) => {
            val lookupVals = (lowIndex to highIndex).toList
            val valsWithTargets: List[(Int, SootUnit)] = (lookupVals.zip(targets))
            Switch(translateExp(key, symtab, anonFuns), valsWithTargets.map(vt => Case(IntLit(vt._1), GoTo(translateLabel(vt._2, symtab)))) ::: List(Default(GoTo(translateLabel(defaultTarget, symtab)))))
          }
          case GLookupSwitchStmt(key: Local, lookupVals: List[Value], targets: List[Stmt], defaultTarget) => {
            val valsWithTargets: List[(Value, SootUnit)] = lookupVals.zip(targets)
            Switch(translateExp(key, symtab, anonFuns), valsWithTargets.map(vt => Case(translateExp(vt._1, symtab, anonFuns), GoTo(translateLabel(vt._2, symtab)))) ::: List(Default(GoTo(translateLabel(defaultTarget, symtab)))))
            // Switch(Id(key.getName), valsWithTargets.map(vt => Case(translateExp(vt._1, anonFuns), TreeSeq(translateUnits(List(vt._2), Nil)))) ::: List(Default(TreeSeq(translateUnits(List(defaultTarget), Nil)))))
            //Id("switch unsupported")
          }
          // IGNORE
          case GThrow(op) => Id("throw unsupported")
          case GExitMonitor(op) => Id("monitors unsupported")
          case GEnterMonitor(op) => Id("monitors unsupported")

          case GStaticInvoke(method, args) => { println("GstaticInvoke::" + method + "::" + args); args.map(a => translateExp(a, symtab, anonFuns)).head }

          case _ => { println("huh " + u); Id("unsupported: " + u) }
        }

        translateUnits(us, result ::: List[Tree](tree), symtab, anonFuns)
      }
      case Nil => result
    }
  }

  private def insertLabels(units: List[SootUnit], symtab: SymbolTable, result: List[Tree], resultWithLabels: List[Tree]): List[Tree] = units match {
    case u :: us => {
      symtab.labels.get(u) match {
        case Some(label) => insertLabels(us, symtab, result.tail, resultWithLabels ::: Label(label) :: result.head :: Nil)
        case None => insertLabels(us, symtab, result.tail, resultWithLabels ::: result.head :: Nil)
      }
    }
    case Nil => resultWithLabels
  }

  // TODO: pass in anonFuns.  Lookup base in the anonFuns map to get a more precise type.
  private def getPossibleReceivers(base: Value, method: SootMethodRef) = {
    if (Modifier.isFinal(method.declaringClass.getModifiers)) {
      method.declaringClass :: Nil
    } else if (Modifier.isFinal(method.resolve.getModifiers)) {
      method.declaringClass :: Nil
    } else {
      base.getType match {
        case t: SootRefType if Modifier.isFinal(t.getSootClass.getModifiers) =>
          // assert method not overridden between method.declaringClass and t
          method.declaringClass :: Nil

        case t: SootRefType => {
          // iterate through all loaded subclasses of t, filtering out those that implement method
          val result = ListBuffer[SootClass]()

          val methodSig = method.name + soot.AbstractJasminClass.jasminDescriptorOf(method)
          val H = Scene.v.getActiveHierarchy

          val queue = new Queue[SootClass]()
          queue += t.getSootClass

          while (!queue.isEmpty) {
            val c = queue.dequeue

            def hasMethod(c: SootClass, methodSig: String): Boolean = {
              for (m <- c.methodIterator) {
                if (!m.isAbstract) {
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

  private def makeFunction(m: SootMethod, result: List[Tree], symtab: SymbolTable, takesThis: Boolean): FunDef = {
    val paramTree = new ListBuffer[Tree]()
    val varTree = new ListBuffer[Tree]()

    if (takesThis)
      paramTree += Formal(symtab.thisParam._2, symtab.thisParam._1)

    if (symtab.params.size == 0)
      println("\n\n PARAM SIZE of " + mangleName(m.getDeclaringClass.getName + m.getName) + " is 0\n\n")

    for (i <- 0 until symtab.params.size /* m.getParameterCount */ ) {
      symtab.params.get(i) match {
        case Some((id, typ)) => paramTree += Formal(typ, id)
        case None => throw new RuntimeException("crap")
      }
    }

    if (symtab.kernelMethod) { 
      for (i <- Kernel.globalArgs) {
        val (t: Tree, s: String) = translateType("global", i._2, i._1)
        paramTree += Formal(t, s)
      }

      for (i <- Kernel.localArgs) {
        val (t: Tree, s: String) = translateType("local", i._2, i._1)
        paramTree += Formal(t, s)
      }
    }

    println("#### localArgs = " + Kernel.localArgs.length)
    

    for ((id: Id, typ: Tree) <- symtab.locals)
      varTree += VarDef(typ, id)

    // kernel methods need to populate group/item structs, later these may become parts of 
    // environments of global and item
    if (symtab.kernelMethod) {
      varTree += Assign(Select(Id("_group_desc"), Id("id")), Call(Id("get_group_id"), IntLit(0)))
      varTree += Assign(Select(Id("_group_desc"), Id("size")), Call(Id("get_global_size"), IntLit(0)))
      
      varTree += Assign(Select(Id("_item_desc"), Id("id")), Call(Id("get_local_id"), IntLit(0)))
      varTree += Assign(Select(Id("_item_desc"), Id("size")), Call(Id("get_local_size"), IntLit(0)))
      varTree += Assign(Select(Id("_item_desc"), Id("globalId")), Call(Id("get_global_id"), IntLit(0)))
    }


    for (((id: Id), (typ: Tree, size: IntLit)) <- symtab.arrays)
      varTree += ArrayDef(id, typ, size)

    FunDef(translateType(m.getReturnType), Id(methodName(m)), paramTree.toList, (varTree.toList ::: result).toArray: _*)
  }
}
