package firepile.compiler

import scala.tools.scalap._
import scala.tools.scalap.{ Main => Scalap }
// import scala.tools.scalap.scalax.rules.scalasig._

import scala.reflect._
import firepile.tree.Trees.{Tree => CLTree}

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
import firepile.tree.Reflect2CL

import firepile.compiler.util.TypeFlow.getSupertypes
// import firepile.tree.Trees._
import firepile.Kernel
// import firepile.tree.Trees.{ Seq => TreeSeq }
import scala.Seq
import soot.jimple.{
  FloatConstant,
  DoubleConstant,
  IntConstant,
  LongConstant,
  StringConstant
}
import firepile.compiler.GrimpUnapply._
import firepile.Marshaling._
import firepile.BBArrayMarshal
import firepile.Device

import scala.collection.mutable.HashMap

import scala.reflect.generic._


case class DummyTree(name: String) extends Tree
case class Struct(name: String, elems: List[Tree]) extends Tree
case class EmptyTree() extends Tree
case class ArrayDef(sym: Symbol, dim: Tree) extends Tree
case class Return(exp: Tree) extends Tree
case class TypeDef(old: Type, nw: Type) extends Tree
case class FunctionDec(ret: Type, name: String, formals: List[Symbol]) extends Tree


object JVM2Reflect {

  def main(args: Array[String]) = {
    if (args.length != 2) {
      println("usage: firepile.compile.JVM2Reflect className methodSig")
      sys.exit(1)
    }

    val className = args(0)
    val methodSig = args(1)

    val tree = compileRoot(className, methodSig, List[Marshal[_]]())

    
    println("Printing tree")
    for (t <- tree) println(t.toString)
    

    tree
  }

  private val makeCallGraph = true
  private var activeHierarchy: Hierarchy = null
  private var ids = Array(false, false, false)
  private var argsByIndex = new ListBuffer[(Boolean,Type)]()

  setup

  def compileRoot(className: String, methodSig: String, argMarshals: List[Marshal[_]], dev: Device = null): List[CLTree] = {
    // println("compiling " + className + "." + methodSig)
    /*
    println("arg types: " + argMarshals.map(am => am match {
        case bbm: BBArrayMarshal[_] => "BB:" + bbm.fixedSizeMarshalMM.manifest
        case fsm: { def fixedSizeMarshalMM: FixedSizeMarshal[_] } => "FM:" + fsm.fixedSizeMarshalMM.manifest
        case x => "unknown marshal " + x
      }))
    */

    println("argMarshals.length = " + argMarshals.length)
    for (am <- argMarshals) {
      // Need to add a case for regular Scala Array??
      am match {
        case bbm: BBArrayMarshal[_] => {
          argsByIndex += Tuple2(true, translateType(bbm.fixedSizeMarshalMM.manifest.toString))
        }
        case fsm: { def fixedSizeMarshalMM: FixedSizeMarshal[_] } => argsByIndex += Tuple2(false, translateType(fsm.fixedSizeMarshalMM.manifest.toString))
        case x => { }
      }
    }

    println("argsByIndex.length = " + argsByIndex.length)

    if (dev != null && dev.memConfig != null)
      kernelDim = dev.memConfig.globalSize.size

    try {
      addRootMethodToWorklist(className, methodSig)
      if (makeCallGraph) {
        buildCallGraph
        optimizeCallGraph
      }
      // println("before process work list")
      val proc = processWorklist
      // println("after process work list")


      println("Result Reflect Tree:")
      for (t <- proc) println(t.toString)
     
      println("Result CLTree:")
      for (t <- proc) println(Reflect2CL.toCLTree(t))

      println("\nResult CL:")
      for (t <- proc) println(Reflect2CL.toCLTree(t).toCL)
      

      // proc.toList
      val clTrees = new ListBuffer[CLTree]()
      for (t <- proc) clTrees += Reflect2CL.toCLTree(t)

      clTrees.toList
    } catch {
      case e: ClassNotFoundException => {
        println("Class not found: " + e.getMessage)
        e.printStackTrace
        Nil
      }
    }
  }

  def reflectTreeToCL(t: Tree): String = {
     t.toString
  }

  def compileMethod(className: String, methodSig: String): List[Tree] = {
    // println("compiling " + className + "." + methodSig)
    try {
      addMethodToWorklist(className, methodSig)
      if (makeCallGraph) {
        buildCallGraph
        optimizeCallGraph
      }
      // println("before process work list")
      val proc = processWorklist
      // println("after process work list")

      /*
      println("Result CL:")
      for (t <- proc) println(t.toCL)
      */

      proc.toList
    } catch {
      case e: ClassNotFoundException => {
        println("Class not found: " + e.getMessage)
        e.printStackTrace
        Nil
      }
    }
  }

  private def setup = {
    // might be useful if you want to relate back to source code
    Options.v.set_keep_line_number(true)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.setPhaseOption("cg", "safe-forname:false")
    Options.v.set_full_resolver(true)
    // you can set context-sensitivity of call graph with this (more time consuming)
    // Options.v.setPhaseOption("cg", "context:1cfa")
    Options.v.setPhaseOption("cg", "verbose:true")

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

  case class CompileMethodTask(method: SootMethodRef, takesThis: Boolean, anonFuns: List[(Int, Value)]) extends Task {
    def run = {
      // println("CompileMethodTask.run")
      val m = method
      val anonFunsLookup = new HashMap[String, Value]()

      // Force the class's method bodies to be loaded.
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.HIERARCHY)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.SIGNATURES)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.BODIES)

      compileMethod(m.resolve, 0, takesThis, anonFunsLookup) match {
          case t => t :: Nil
      }
    }
  }

  case class CompileRootMethodTask(method: SootMethodRef, takesThis: Boolean, anonFuns: List[(Int, Value)]) extends Task {
    def run = {
      // println("CompileRootMethodTask.run()")
      val m = method
      val anonFunsLookup = new HashMap[String, Value]()
      kernelMethod = true

      // Force the class's method bodies to be loaded.
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.HIERARCHY)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.SIGNATURES)
      Scene.v.tryLoadClass(m.declaringClass.getName, SootClass.BODIES)

      compileMethod(m.resolve, 0, takesThis, anonFunsLookup) match {
        case null => Nil
        case Function(params, Block(body, ident@Ident(Method(funName,retTyp)))) => {
          val popArrayStructs = ListBuffer[Tree]()
          val frmls = params.flatMap(f => f match {
              case LocalValue(_, name, x@NamedType(fullname)) if fullname.endsWith("Array") => {
                val rawTypeName = fullname.substring(fullname.indexOf('_')+1, fullname.lastIndexOf("Array"))
                fullname match {
                  case fn if fn.startsWith("g") => { // handle global arrays
                    popArrayStructs += Assign(Select(Select(Ident(LocalValue(NoSymbol,"_this_kernel",NamedType("kernel"))),Field(name,x)),Field("data",NamedType(rawTypeName))), Ident(LocalValue(NoSymbol,name + "_data",NamedType(rawTypeName))))
                    popArrayStructs += Assign(Select(Select(Ident(LocalValue(NoSymbol,"_this_kernel",NamedType("kernel"))),Field(name,x)),Field("length",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), Ident(LocalValue(NoSymbol,name + "_len",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))))
                    List(LocalValue(NoSymbol,"g_" + name + "_data",NamedType(rawTypeName)),LocalValue(NoSymbol,"C_" + name + "_len",PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))
                  }
                  case fn if fn.startsWith("l") => { // handle global arrays
                    popArrayStructs += Assign(Select(Select(Ident(LocalValue(NoSymbol,"_this_kernel",NamedType("kernel"))),Field(name,x)),Field("data",NamedType(rawTypeName))), Ident(LocalValue(NoSymbol,name + "_data",NamedType(rawTypeName))))
                    popArrayStructs += Assign(Select(Select(Ident(LocalValue(NoSymbol,"_this_kernel",NamedType("kernel"))),Field(name,x)),Field("length",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), Ident(LocalValue(NoSymbol,name + "_len",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))))
                    List(LocalValue(NoSymbol,"l_" + name + "_data",NamedType(rawTypeName)),LocalValue(NoSymbol,"C_" + name + "_len",PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))
                  }
                }
              }
              case LocalValue(_, name, nt) => { // constants, scalars
                popArrayStructs += Assign(Select(Ident(LocalValue(NoSymbol,"_this_kernel",NamedType("kernel"))),Field(name,nt)), Ident(LocalValue(NoSymbol, name, nt)))
                nt match {
                  case NamedType(n) if n.startsWith("g_") => List(LocalValue(NoSymbol, "g_"+name, NamedType(n.substring(n.indexOf("g_")+2))))
                  case NamedType(n) if n.startsWith("l_") => List(LocalValue(NoSymbol, "l_"+name, NamedType(n.substring(n.indexOf("l_")+2))))
                  case NamedType(n) if n.startsWith("p_") => List(LocalValue(NoSymbol, "p_"+name, NamedType(n.substring(n.indexOf("p_")+2))))
                  case NamedType(n) if n.startsWith("c_") => List(LocalValue(NoSymbol, "c_"+name, NamedType(n.substring(n.indexOf("c_")+2))))
                  case _ => List(LocalValue(NoSymbol,name,nt))
                }
              }
              case x => List(x)
            })

            Function(frmls, Block(List(ValDef(LocalValue(NoSymbol,"_this_kernel", NamedType("kernel_ENV")),EmptyTree())) ::: popArrayStructs.toList ::: body,
                         ident)) :: Nil
                        

        }
        case t => t :: Nil
      }

    }
  }

  private val worklist = new Worklist[Task]
  var kernelMethodName = ""

  private def addRootMethodToWorklist(className: String, methodSig: String): Unit = {
    // println("\n\nADD ROOT METHOD TO WORKLIST\n\n")
    // Set up the class we're working with
    val c = Scene.v.loadClassAndSupport(className)
    if (makeCallGraph) {
      Scene.v.loadNecessaryClasses
      c.setApplicationClass
    }

    // Retrieve the method and its body
    // println(" Method Iterator ")
    for (m <- c.methodIterator) {
      // println("m.getName " + m.getName)
      val sig = if (methodSig.equals("apply")) m.getName else m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef)
      // println("trying " + sig)
      if (sig.equals(methodSig) && m.getParameterTypes.forall(p => !p.toString.equals("java.lang.Object"))) {
        worklist += CompileRootMethodTask(m.makeRef, false, null)
        kernelMethodName = methodName(m)
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
    val preambleArrays = ListBuffer[Tree]()
    val preambleEnvs = ListBuffer[Tree]()

    envstructs.structs += NamedType("firepile_Group") ->
      List(Struct("_firepile_Group", List(ValDef(LocalValue(NoSymbol, "id", PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()),
                                         ValDef(LocalValue(NoSymbol, "size",PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()))))
    envstructs.structs += NamedType("firepile_Item") ->
      List(Struct("_firepile_Item", List(ValDef(LocalValue(NoSymbol, "id",PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()), 
                                        ValDef(LocalValue(NoSymbol, "size",PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()), 
                                        ValDef(LocalValue(NoSymbol, "globalId",PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()))))
  
    preambleEnvs += TypeDef(NamedType("_kernel_ENV"), NamedType("kernel_ENV"))
    preambleEnvs += TypeDef(NamedType("_firepile_Group"), NamedType("firepile_Group"))
    preambleEnvs += TypeDef(NamedType("_firepile_Item"), NamedType("firepile_Item"))

    while (!worklist.isEmpty) {
      val task = worklist.dequeue
      val ts = task.run
      ts match {
        case Function(params, Block(body, Ident(Method(funName, retType)))) :: fs => {
          if (!functionDefs.contains(funName)) {
            if (!funName.equals(kernelMethodName))
              functionDefs += funName -> FunctionDec(retType, funName, params)
            results ++= ts
          }
        }
        case _ => results ++= ts
      }
    }

    preambleArrays ++= arraystructs.structs.values.flatten.map(as => TypeDef(NamedType(as.asInstanceOf[Struct].name), NamedType(as.asInstanceOf[Struct].name.substring(1))))


    val tree = arraystructs.dumpArrayStructs ::: preambleArrays.toList ::: envstructs.dumpEnvStructs ::: preambleEnvs.toList ::: functionDefs.values.toList ::: results.toList

    arraystructs.clearArrays
    envstructs.clearEnvs
    // classtab.clearClassTable

    tree
  }

  /*
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

  */

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

  private def compileMethod(m: SootMethod, level: Int, takesThis: Boolean, anonFuns: HashMap[String, Value]): Tree = {
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
      println(u)
    }

    val units = unitBuffer.toList
    /*
    println("Grimp method body:")
    println(units.mkString("\n"))
    */

    val body = translateUnits(units, Nil, symtab, anonFuns)

    val labeled = insertLabels(units, symtab, body, Nil)

    // val fun = makeFunction(m, removeThis(labeled, symtab), symtab, false)

    val fun = makeFunction(m, labeled, symtab, false)

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

    // DummyTree(body.toString)
    fun
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

  var kernelMethod = false
  var kernelDim = 1

  class SymbolTable(val methodName: String) {
    val labels = new HashMap[SootUnit, String]()
    val params = new HashMap[Int, (Symbol, Type)]()
    val mparams = new HashMap[Int, (String, Tree)]()
    val locals = new ListBuffer[ValDef]()
    val arrays = new HashMap[Symbol, (Type /*type*/ , Tree /*size*/ )]()
    var thisParam: (Symbol, Tree) = null
    var kernelMethod = false
    var level: Int = 0

    def addThisParam(typ: SootType, id: Symbol) = {
      /*
      val typUnion = translateType(typ) match {
        case PtrType(v: ValueType) => PtrType(ValueType(v.name + "_intr"))
        case ValueType(name: String) => ValueType(name + "_intr")
        case x => x
      }
      thisParam = (id, typUnion)
      */
    }

    def addParamVar(typ: SootType, index: Int, id: Symbol) = {
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
        case _ => {} // params += index -> (Id(name), ValueType(typ))
      }
    }

    
    def addLocalVar(typ: SootType, id: Symbol) = {
      // assert(!params.contains(id))
      // assert(!locals.contains(id))
      /*
      translateType(typ) match {
        case PtrType(StructType(n)) if n.equals("firepile_Group") => { }
        case PtrType(StructType(n)) if n.equals("firepile_Item") => { }
        case _ => locals += id -> translateType(typ)
      }
      */
      // locals += id -> translateType(typ)
      id match {
        case LocalValue(_, name, _) if name.equals("this") => {}
        case l: LocalValue =>
          if (!locals.contains(ValDef(id, EmptyTree())))
            locals += ValDef(id, EmptyTree())
      }
    }
    

    def addLocalVar(typ: Type, id: Symbol) = {
      // assert(!params.contains(id))
      // assert(!locals.contains(id))
      /*
      typ match {
        case PtrType(StructType(n)) if n.equals("firepile_Group") => { }
        case PtrType(StructType(n)) if n.equals("firepile_Item") => { }
        case _ => locals += id -> typ
      }
      */
      // locals += id -> typ
    }

    def addArrayDef(typ: SootType, id: Symbol, size: Tree) = {
      assert(!locals.contains(id))
      println("------ADD ARRAY DEF")
      arrays += id -> (translateType(typ), size)
    }

    def lastParamIndex = params.size - 1

    var next = -1

    def nextLabel = {
      next += 1
      "lbl_" + next
    }
  }


  def translateType(t: SootType, i: Int = -1): Type = t match {
    case t: SootVoidType => PrefixedType(ThisType(Class("scala")),Class("scala.Any"))
    case t: SootBooleanType => PrefixedType(ThisType(Class("scala")),Class("scala.Boolean")) 
    case t: SootByteType => PrefixedType(ThisType(Class("scala")),Class("scala.Byte"))
    case t: SootShortType => PrefixedType(ThisType(Class("scala")),Class("scala.Short"))
    case t: SootCharType => PrefixedType(ThisType(Class("scala")),Class("scala.Char"))
    case t: SootIntType => PrefixedType(ThisType(Class("scala")),Class("scala.Int"))
    case t: SootLongType => PrefixedType(ThisType(Class("scala")),Class("scala.Long"))
    case t: SootFloatType => PrefixedType(ThisType(Class("scala")),Class("scala.Float"))
    case t: SootDoubleType => PrefixedType(ThisType(Class("scala")),Class("scala.Double"))
    case t: SootRefType => {
      t.getSootClass.getName match {
        case "scala.Tuple2" => PrefixedType(ThisType(Class("scala")),Class("scala.Tuple2"))
        case "scala.Tuple3" => PrefixedType(ThisType(Class("scala")),Class("scala.Tuple3"))
        case "scala.Tuple4" => PrefixedType(ThisType(Class("scala")),Class("scala.Tuple4"))
        case "scala.Tuple5" => PrefixedType(ThisType(Class("scala")),Class("scala.Tuple5"))
        case "scala.Tuple6" => PrefixedType(ThisType(Class("scala")),Class("scala.Tuple6"))
        case "firepile.Spaces$Point1" => PrefixedType(ThisType(Class("scala")),Class("scala.Int"))
        case "firepile.Spaces$Id1" => PrefixedType(ThisType(Class("scala")),Class("scala.Any")) // StructType(mangleName(t.toString))
        case "firepile.util.Unsigned$UInt" => NamedType("firepile.util.Unsigned.UInt") 
// ValueType("unsigned int")
        case "firepile.util.BufferBackedArray$BBArray" => 
          if (kernelMethodName.equals("firepile_tests_DCT8x8__anonfun_DCT8x8_1apply")) 
            arraystructs.addStruct(NamedType("scala.Float"))
          else {
            if (i > -1)
              arraystructs.addStruct(argsByIndex(i)._2) 
            else
              NamedType("firepile.util.BufferBackedArray$BBArray")
          }
        case _ => NamedType(mangleName(t.toString)) // PtrType(StructType(mangleName(t.toString)))
      }
    }
    case t: SootArrayType => { arraystructs.addStruct(translateType(t.getArrayElementType)) }

    //case t: SootArrayType => PtrType(translateType(t.getArrayElementType))
    case t: SootNullType => NamedType("Unmatched:SootNullType")
    case _ => NamedType("UnknownType")
  }

  def translateTypeSimpleName(t: SootType, i: Int = -1): String = t match {
    case t: SootVoidType => "scala.Any"
    case t: SootBooleanType => "scala.Boolean" 
    case t: SootByteType => "scala.Byte"
    case t: SootShortType => "scala.Short"
    case t: SootCharType => "scala.Char"
    case t: SootIntType => "scala.Int"
    case t: SootLongType => "scala.Long"
    case t: SootFloatType => "scala.Float"
    case t: SootDoubleType => "scala.Double"
    case t: SootRefType => {
      t.getSootClass.getName match {
        case "scala.Tuple2" => "scala.Tuple2"
        case "scala.Tuple3" => "scala.Tuple3"
        case "scala.Tuple4" => "scala.Tuple4"
        case "scala.Tuple5" => "scala.Tuple5"
        case "scala.Tuple6" => "scala.Tuple6"
        case "firepile.Spaces$Point1" => "scala.Int"
        case "firepile.Spaces$Id1" => "scala.Int" // StructType(mangleName(t.toString))
        case "firepile.util.Unsigned$UInt" => "firepile.util.Unsigned.UInt"
// ValueType("unsigned int")
        case "firepile.util.BufferBackedArray$BBArray" => throw new RuntimeException("Can't handle BBArrays yet")
        /*
          if (kernelMethodName.equals("firepile_tests_DCT8x8__anonfun_DCT8x8_1apply")) 
            arraystructs.addStruct(ValueType("float"))
          else
            arraystructs.addStruct(argsByIndex(i)._2) 
        */
        case _ => mangleName(t.toString) // PtrType(StructType(mangleName(t.toString)))
      }
    }
    case t: SootArrayType => arraystructs.addStruct(translateType(t.getArrayElementType)).asInstanceOf[NamedType].fullname
    case t: SootNullType => "Unmatched:SootNullType"
    case _ => "UnknownType"
  }


  private def translateType(mem: String, typ: String, name: String): (Type, String) = {
    val memType = mem match { 
      case "global" => "g_" 
      case "local" => "l_" 
      case _ => ""
    }
    typ match {

      case "java.lang.String" => (NamedType(memType + "java_lang_String"), mangleName(name))
      case "firepile.util.Unsigned.UInt" => (NamedType(memType + "firepile_util_Unsigned_UInt"), mangleName(name))
      case "int" => (NamedType(memType + "scala_Int"), mangleName(name))
      case "float" => (NamedType(memType + "scala_Float"), mangleName(name))
      case "long" => (NamedType(memType + "scala_Long"), mangleName(name))
      case "double" => (NamedType(memType + "scala_Double"), mangleName(name))
      case "float[]" => (NamedType(memType + "scala_Float_Array"), mangleName(name)) // (MemType(memType, ValueType("float")), "*" + mangleName(name))
      case "int[]" => (NamedType(memType + "scala_Int_Array"), mangleName(name))
      case "long[]" => (NamedType(memType + "scala_Long_Array"), mangleName(name))
      case "double[]" => (NamedType(memType + "scala_Double_Array"), mangleName(name))

      case x => (NamedType(memType + mangleName(typ)), mangleName(name))
    }
  }


  private def translateType(memType: String, typ: SootType, name: String, idx: Int): (Type, String) = translateType(typ, idx) match {
      case NamedType(n) if n.endsWith("Array") => memType match {
        case "local" => (NamedType("l_" + n), mangleName(name))
        case "global" => (NamedType("g_" + n), mangleName(name))
      }
      case PrefixedType(ThisType(Class("scala")),Class(s)) => memType match {
        case "local" =>  (NamedType("l_" + s), mangleName(name))
        case "global" =>  (NamedType("g_" + s), mangleName(name))
      }
      case _ => (translateType(typ), mangleName(name))
      // case _ => (MemType(memType, translateType(typ)), mangleName(name))
  }

  private def translateType(t: ScalaVarDef): Type = t.fieldScalaType match {
    case NamedTyp(s: String) => s match {
      case "scala.Unit" =>  PrefixedType(ThisType(Class("scala")),Class(s)) 
      case "scala.Boolean" =>  PrefixedType(ThisType(Class("scala")),Class(s))
      case "scala.Byte" => PrefixedType(ThisType(Class("scala")),Class(s))
      case "scala.Char" => PrefixedType(ThisType(Class("scala")),Class(s))
      case "scala.Short" => PrefixedType(ThisType(Class("scala")),Class(s))
      case "scala.Int" => PrefixedType(ThisType(Class("scala")),Class(s))
      case "scala.Long" => PrefixedType(ThisType(Class("scala")),Class(s))
      case "scala.Float" => PrefixedType(ThisType(Class("scala")),Class(s))
      case "scala.Double" => PrefixedType(ThisType(Class("scala")),Class(s))
      case "java.lang.String" => PrefixedType(ThisType(Class("scala")),Class("java.lang.String"))
      case "firepile.util.Unsigned.UInt" => NamedType("Firepile UInt")
      case "firepile.Spaces$Id1" => NamedType("Spaces$Id1") // StructType(mangleName(t.toString))
      case "int" => NamedType("ValueType(int)")
      case "float[]" => NamedType("PtrType(ValueType(float))")
      case "int[]" => NamedType("PtrType(ValueType(int))")
      case "long[]" => NamedType("PtrType(ValueType(long))")
      case "double[]" => NamedType("PtrType(ValueType(double))")

      case x => NamedType("PtrType(ValueType("+x+"))")
    }
    case _ => NamedType("PtrType(ValueType("+t.fieldTypeAsString+"))")
  }

  
  private def translateType(s: String): Type = s match {
    case "Unit" =>  PrefixedType(ThisType(Class("scala")),Class("scala."+s)) 
    case "Boolean" =>  PrefixedType(ThisType(Class("scala")),Class("scala."+s))
    case "Byte" => PrefixedType(ThisType(Class("scala")),Class("scala."+s))
    case "Char" => PrefixedType(ThisType(Class("scala")),Class("scala."+s))
    case "Short" => PrefixedType(ThisType(Class("scala")),Class("scala."+s))
    case "Int" => PrefixedType(ThisType(Class("scala")),Class("scala."+s))
    case "Long" => PrefixedType(ThisType(Class("scala")),Class("scala."+s))
    case "Float" => PrefixedType(ThisType(Class("scala")),Class("scala."+s))
    case "Double" => PrefixedType(ThisType(Class("scala")),Class("scala."+s))
    case x => NamedType(x)
  }
 

  class ArrayStructs {
    val structs = new HashMap[Type /* type */ , List[Tree] /* struct rep */ ]()

    def addStruct(typ: Type): Type = {
      val arrayTyp = typ match {
        case NamedType(name) => name
        case PrefixedType(ThisType(Class("scala")),Class(typ)) => typ
        case _ => throw new RuntimeException("Unknown array type: " + typ)
      }
      if (!structs.contains(typ)) {
        structs += typ -> List(Struct("_g_" + mangleName(arrayTyp).replaceAll(" ", "_") + "Array", List(ValDef(LocalValue(NoSymbol,"length", PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()), ValDef(LocalValue(NoSymbol, "data", NamedType("g_"+arrayTyp)),EmptyTree()))),
                               Struct("_l_" + mangleName(arrayTyp).replaceAll(" ", "_") + "Array", List(ValDef(LocalValue(NoSymbol,"length", PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()), ValDef(LocalValue(NoSymbol, "data", NamedType("l_"+arrayTyp)),EmptyTree()))),
                               Struct("_c_" + mangleName(arrayTyp).replaceAll(" ", "_") + "Array", List(ValDef(LocalValue(NoSymbol,"length", PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()), ValDef(LocalValue(NoSymbol, "data", NamedType("c_"+arrayTyp)),EmptyTree()))),
                               Struct("_p_" + mangleName(arrayTyp).replaceAll(" ", "_") + "Array", List(ValDef(LocalValue(NoSymbol,"length", PrefixedType(ThisType(Class("scala")),Class("scala.Int"))),EmptyTree()), ValDef(LocalValue(NoSymbol, "data", NamedType("p_"+arrayTyp)),EmptyTree()))))
      }

      NamedType(arrayTyp.replaceAll(" ", "_") + "Array")
    }

    def dumpArrayStructs = {
      // println("ARRAY STRUCTS CL:")
      // structs.values.flatten.foreach((cl: Tree) => println(cl.toCL))
      structs.values.toList.flatten
    }

    def clearArrays = structs.clear
  }

  class EnvStructs {
    val structs = new HashMap[Type /* type */ , List[Tree] /* struct rep */ ]()

    def addStruct(typ: Type): Type = {
      val envTyp = typ match {
        case NamedType(_) => typ
        case _ => throw new RuntimeException("Unknown env type: " + typ)
      }
      if (!structs.contains(typ)) {
        structs += typ -> List(Struct("_"+envTyp.asInstanceOf[NamedType].fullname.replaceAll(" ", "_") + "_ENV", List[Tree]()))
      }
      NamedType("_"+envTyp.asInstanceOf[NamedType].fullname.replaceAll(" ", "_") + "_ENV")
    }

    def append(envTyp: Type, varr: ValDef) = {
      structs(envTyp).head match {
        case Struct(name, vars) => 
          if (!vars.contains(varr))
            structs(envTyp) = List(Struct(name, varr :: vars))
        case _ => throw new RuntimeException("Existing environment struct not found")
      }
    }

    def contains(envTyp: Type, name: String) = {
      structs(envTyp).head match {
        case Struct(sName, vars) => vars.exists(v => { v.asInstanceOf[ValDef].sym.asInstanceOf[LocalValue].name.equals(mangleName(name)) })
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
        case GXor(op1, op2) => DummyTree("^") // Bin(op1, "^", op2)
        case GOr(op1, op2) => DummyTree("|") // Bin(op1, "|", op2)
        case GAnd(op1, op2) => DummyTree("&")  // Bin(op1, "&", op2)

        case GUshr(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$rshift",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))
  // Bin(Cast(ValueType("unsigned"), op1), ">>", op2)
        case GShr(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$rshift",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))

                                                                     // Bin(op1, ">>", op2)
        case GShl(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$lshift",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))
 // Bin(op1, "<<", op2)
        /*
        case GAdd(op1, op2) => Apply(Select(Ident(LocalValue(NoSymbol,op1.asInstanceOf[Local].getName,translateType(op1.getType))),
                                            Method("scala.Float.$plus",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))),
                                     List(Ident(LocalValue(NoSymbol,op2.asInstanceOf[Local].getName,translateType(op2.getType)))))
        */
        case GAdd(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$plus",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))
       /*
          DummyTree("+"),
                                            List(LocalValue(NoSymbol,op2.asInstanceOf[Local].getName,translateType(op2.getType)))),
                                      LocalValue(NoSymbol,op1.asInstanceOf[Local].getName,translateType(op1.getType))) // Bin(op1, "+", op2)

        Apply(Select(Ident(LocalValue(NoSymbol,a,PrefixedType(ThisType(Class(scala)),Class(scala.Float)))),Method(scala.Float.$plus,MethodType(List(LocalValue(NoSymbol,x,PrefixedType(ThisType(Class(scala)),Class(scala.Float)))),PrefixedType(ThisType(Class(scala)),Class(scala.Float))))),List(Ident(LocalValue(NoSymbol,b,PrefixedType(ThisType(Class(scala)),Class(scala.Float))))))
        */
        case GSub(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$minus",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))       // Bin(op1, "-", op2)
        case GMul(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$times",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, "*", op2)
        case GDiv(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$div",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, "/", op2)
        case GRem(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$percent",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, "%", op2)

        case GEq(GCmpg(op1, op2), GIntConstant(0)) => DummyTree("==") // Bin(op1, "==", op2)
        case GEq(GCmpl(op1, op2), GIntConstant(0)) => DummyTree("==") // Bin(op1, "==", op2)
        case GEq(GCmp(op1, op2), GIntConstant(0)) => DummyTree("==") // Bin(op1, "==", op2)
        case GEq(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$eq",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, "==", op2)

        case GNe(GCmpg(op1, op2), GIntConstant(0)) => DummyTree("!=") // Bin(op1, "!=", op2)
        case GNe(GCmpl(op1, op2), GIntConstant(0)) => DummyTree("!=") // Bin(op1, "!=", op2)
        case GNe(GCmp(op1, op2), GIntConstant(0)) => DummyTree("!=") // Bin(op1, "!=", op2)
        case GNe(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$neq",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, "!=", op2)

        case GGt(GCmpg(op1, op2), GIntConstant(0)) => DummyTree(">") // Bin(op1, ">", op2)
        case GGt(GCmpl(op1, op2), GIntConstant(0)) => DummyTree(">") // Bin(op1, ">", op2)
        case GGt(GCmp(op1, op2), GIntConstant(0)) => DummyTree(">") // Bin(op1, ">", op2)
        case GGt(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$greater",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, ">", op2)

        case GLt(GCmpg(op1, op2), GIntConstant(0)) => DummyTree("<") // Bin(op1, "<", op2)
        case GLt(GCmpl(op1, op2), GIntConstant(0)) => DummyTree("<") // Bin(op1, "<", op2)
        case GLt(GCmp(op1, op2), GIntConstant(0)) => DummyTree("<") // Bin(op1, "<", op2)
        case GLt(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$less",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, "<", op2)

        case GLe(GCmpg(op1, op2), GIntConstant(0)) => DummyTree("<=") // Bin(op1, "<=", op2)
        case GLe(GCmpl(op1, op2), GIntConstant(0)) => DummyTree("<=")  // Bin(op1, "<=", op2)
        case GLe(GCmp(op1, op2), GIntConstant(0)) => DummyTree("<=") // Bin(op1, "<=", op2)
        case GLe(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$leq",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, "<=", op2)

        case GGe(GCmpg(op1, op2), GIntConstant(0)) => DummyTree(">=") // Bin(op1, ">=", op2)
        case GGe(GCmpl(op1, op2), GIntConstant(0)) => DummyTree(">=") // Bin(op1, ">=", op2)
        case GGe(GCmp(op1, op2), GIntConstant(0)) => DummyTree(">=")  // Bin(op1, ">=", op2)
        case GGe(op1, op2) => Apply(Select(op1,
                                            Method(translateTypeSimpleName(op1.getType)+".$geq",MethodType(List(LocalValue(NoSymbol,"x",translateType(op1.getType))),
                                                                       translateType(op1.getType)))), List(op2))  // Bin(op1, ">=", op2)

        case GCmpg(op1, op2) => DummyTree("unimplemented:cmpg") // Id("unimplemented:cmpg")
        case GCmpl(op1, op2) => DummyTree("unimplemented:cmpl") // Id("unimplemented:cmpl")
        case GCmp(op1, op2) => DummyTree("unimplemented:cmp")  // Id("unimplemented:cmp")

        case GNeg(op) => DummyTree("Un(-)")  // Un("-", op)
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

        // println("::v::" + v)
        v match {
          // Must be first
          // case LibraryCall(t) => t

          // Must be second
          case Operator(t) => t

          case GNullConstant() => DummyTree("NULL") // Id("NULL")

          case GIntConstant(value) => Literal(value) // DummyTree("GIntConstant")
          case GLongConstant(value) => DummyTree("GLongConstant")
          case GFloatConstant(value) => Literal(value) // DummyTree("GFloatConstant")
          case GDoubleConstant(value) => DummyTree("GDoubleConstant")
          case GUIntConstant(value) => DummyTree("GUIntConstant")
          case GArrayLength(op) => DummyTree("GArrayLength") // Select(op, "length")

          case GCast(op, castTyp) => DummyTree("GCast") // Cast(translateType(castTyp), translateExp(op, symtab, anonFuns))

          // IGNORE
          case GInstanceof(op, instTyp) => DummyTree("unimplemented:instanceof")

          // IGNORE
          case GNew(newTyp) => { /* classtab.addClass(new SootClass(newTyp.asInstanceOf[SootType].toString));*/ DummyTree("unimplemented:new") }

          // IGNORE
          case GNewArray(newTyp, size) => DummyTree("GNewArray") // Id("unimplemented:newarray")
          // IGNORE
          case GNewMultiArray(newTyp, sizes) => DummyTree("GNewMultiArray") // Id("unimplemented:newmultiarray")

          case GNewInvoke(baseTyp, method@SMethodRef(_, "<init>", _, _, _), args) => {

            //println("baseName:::" + baseTyp.getSootClass.getName)
            //for (i <- args) println(" args:" + i)
            if (baseTyp.getSootClass.getName.equals("scala.Tuple2")) {
/*
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
            */
              DummyTree("GNewInvoke")
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
              // Call(Id("makeClosure"), args.map(a => translateExp(a, symtab, anonFuns)))
              DummyTree("makeClosure")
            } else {
              // worklist += CompileMethodTask(method)
              // throw new RuntimeException("Cannot create new instances of classes")

              // TODO: Some things call new such as java.lang.Float.valueOf, need a way to handle this
              // Call(Id("_init_"), Call(Id("new_" + mangleName(baseTyp.toString)), args.map(a => translateExp(a, symtab, anonFuns))))
              DummyTree("Call _init")
            }
          }

          //case GInstanceFieldRef(instBase, fieldRef) => { println(" GInstanceFieldRef::"+instBase+"::"+fieldRef); Id(fieldRef.name) }
          case GStaticInvoke(method@SMethodRef(_, "boxToFloat", _, _, _), args) => args.map(a => translateExp(a, symtab, anonFuns)).head
          case GStaticInvoke(method@SMethodRef(_, "unboxToFloat", _, _, _), args) => args.map(a => translateExp(a, symtab, anonFuns)).head
          case GStaticInvoke(method, args) => {
            // println(" static method:" + method)
            // println(" static method NAME:" + method.name)
            // println("methodName(method):" + methodName(method))
            // for (i <- args) println("arg::" + i)

            worklist += CompileMethodTask(method)
            // classtab.addClass(method.declaringClass)
            // Call(Id(method.name), args.map(a => translateExp(a, symtab, anonFuns)))
            DummyTree("Call " + method.name)
          }
          case GSpecialInvoke(base: Local, method, args) => {
            worklist += CompileMethodTask(method, true)
            //Call(Select(base, method.name), Id("_this") :: args.map(a => translateExp(a)))
            // classtab.addClass(method.declaringClass)
            // Call(Id(methodName(method)), Id("_this") :: args.map(a => translateExp(a, symtab, anonFuns)))
            DummyTree("GSpecialInvoke")
          }

          // Ignore Point conversion to int for now
          case GVirtualInvoke(_, method@SMethodRef(_, "point12int", _, _, _), args) => args.map(a => translateExp(a, symtab, anonFuns)).head

          case GVirtualInvoke(base, method, args) => {
            handleIdsVirtualInvoke(v, symtab, anonFuns) match {
              case Some(x) => x
              // case _ => defaultGVirtualInvoke(base, method, args, symtab, anonFuns)
              case _ => {
                DummyTree("GVirtualInvoke:" + method.name)

              }
            }
          }

          //g$1.<firepile.Group: scala.collection.immutable.List items()>()::<: int ()>::List()

          case GInterfaceInvoke(base: GVirtualInvokeExpr, SMethodRef(SClassName("scala.collection.SeqLike"), "size", _, _, _), _) => { 
              val fieldName = base.getBase match {
                case b: soot.grimp.internal.GInstanceFieldRef => mangleName(b.getField.getName) // return Select(Id(mangleName(b.getField.getName)), Id("size"))
                case b: Local => mangleName(b.getName) // return Select(Id(b.getName), Id("size"))
                case _ => throw new RuntimeException("Getting size of some unknown collection")
              }
              // Select(ArrayAccess(Id("item"), IntLit(0)), Id("size"))
              // DummyTree("GInterfaceInvoke for SeqLike.size")
              /*
              Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("scala.collection")),Class("scala.collection.SeqLike")))),
                          Method("scala.collection.SeqLike.size",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0)))
              */
              Select(Apply(Select(Ident(LocalValue(NoSymbol, "item", NamedType("firepile_Group"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0))), Field("size", PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))

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

                  /*
              for (applyMethod <- applyMethods) {
                println("Found apply method: " + applyMethod.getSignature)
              }
              */

                  val fd = compileMethod(applyMethods.head, symtab.level + 1, false, anonFuns)

                  // symtab.addInlineParams(fd.formals)

                  worklist += CompileMethodTask(applyMethods.head)
                  // ClosureCall(Id(mangleName(closureTyp.toString) + method.name), closureArgs.map(ca => translateExp(ca, symtab, anonFuns)) ::: args.map(a => translateExp(a, symtab, anonFuns)))
                  DummyTree("ClosureCall")

                // case _ => Call(Select(b, method.name), args.map(a => translateExp(a, symtab, anonFuns)))
                case _ => DummyTree("GInterfaceInvoke") // Call(Id(method.name), args.map(a => translateExp(a, symtab, anonFuns)))
              }

              /*
          if(base.getType.toString.startsWith("scala.Function"))
            ClosureCall(Id(base.getName), args.map(a => translateExp(a, anonFuns)))
          else
            Call(Select(base, method.name), args.map(a => translateExp(a, anonFuns)))
          */
            }
            case GInstanceFieldRef(instBase, fieldRef) => {

              // println(" GInstanceFieldRef::" + instBase + "::" + fieldRef)
              // printf("GInstanceFieldRef:: instBase name = " + instBase.asInstanceOf[Local].getName + " with method name " + method.name)
              // TODO: comment
              // Handle accessing captured variables.  Assumes captured variable 'x' is 'this.x$1'
              // TODO: eliminate (if possible) the assumptions here about the naming conventions of variables.
              if (fieldRef.`type`.toString.startsWith("scala.Function")) {
                // println("======= LOOKING FOR fieldRef: " + fieldRef.name + " in ")
                anonFuns.keys.foreach(k => print(k + " "))
                anonFuns.get(fieldRef.name.takeWhile(_ != '$')) match {
                  // matched 'this.x$1.apply(args)' where 'x$1' is a captured variable 'x'
                  // and 'x' is 'new anonfun$1(closureArgs)'
                  // ->
                  // anonfun_dollar1_apply(env, args)
                  // and also translate the body of that method
                  case Some(GNewInvoke(closureTyp, closureMethod, closureArgs)) => {
                    // println("CLOSURE METHOD: " + fieldRef.name)
                    // TODO:  Need to translate methods with java.lang.Object parameters also, can't always just filter them out
                    val applyMethods = closureTyp.getSootClass.getMethods.filter(mn => mn.getName.equals(method.name) && !mn.getParameterType(0).toString.equals("java.lang.Object"))

                    /*
                for (applyMethod <- applyMethods) {
                  println("Found apply method: " + applyMethod.getSignature)
                }
                */

                    worklist += CompileMethodTask(applyMethods.head)
                    // ClosureCall(Id(mangleName(closureTyp.toString + method.name)), args.map(a => translateExp(a, symtab, anonFuns)))
                    DummyTree("GInstanceFieldRef on scala.Function")
                  }
                  //case _ => Select(base, mangleName(fieldRef.name)) // TODO: punt
                  
                  case None => Select(Ident(Class(translateTypeSimpleName(base.getType))), Field(mangleName(fieldRef.name), translateType(fieldRef.`type`)))
 // DummyTree("GInstanceFieldRef") // Id(mangleName(fieldRef.name))
                }
                //} else Select(base, mangleName(fieldRef.name)) // TODO: punt
              } else Select(Ident(Class(translateTypeSimpleName(base.getType))), Field(mangleName(fieldRef.name), translateType(fieldRef.`type`)))
 // DummyTree("GInstanceFieldRef") // Id(mangleName(fieldRef.name))

            }

            case GStaticInvoke(method, args) => DummyTree("STATICINVOKE inside INTERFACE INVOKE") // Id("STATICINVOKE inside INTERFACE INVOKE")

            //Changes here
            case GStaticFieldRef(_) => DummyTree("static field ref") // Id("static field ref")
            case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Space"), "groups", _, _, _), _) => {
                var applyM: SootMethod = null
                for (a <- args) {
                  // println("Getting SootClass for: " + a.getType.toString)
                  val sootcls = Scene.v.getSootClass(a.getType.toString)
                  val applyMethods = sootcls.getMethods.filter(mn => mn.getName.equals("apply")  && !mn.getParameterType(0).toString.equals("java.lang.Object"))
                  // println("number of methods = " + sootcls.getMethodCount)

                  // println("Apply methods found: " + applyMethods.length)
                  for (applyMethod <- applyMethods) {
                    // println("Found method: " + applyMethod.getSignature)
                    worklist += CompileMethodTask(applyMethod.makeRef)
                    compileMethod(applyMethod, 0, false, anonFuns)
                    applyM = applyMethod
                  }

                }
                // Call group function with group ID struct
                // Call(Id(methodName(applyM)), Id("_group_desc"), Id("_this_kernel"))
                // DummyTree("GVirtualInvoke:firepile.Space.groups")

                Apply(Select(Ident(LocalValue(NoSymbol,"firepile.Space",PrefixedType(ThisType(Class("firepile")),Class("firepile.Space")))),
      Method(methodName(applyM),MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Any")))), PrefixedType(ThisType(Class("scala")),Class("scala.Any"))))), List(Ident(LocalValue(NoSymbol,"_group_desc",NamedType("firepile.Group"))), Ident(LocalValue(NoSymbol,"_this_kernel",NamedType("kernel_ENV")))))
            }
            case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Group"), "items", _, _, _), _) => {
                var applyM: SootMethod = null
                for (a <- args) {
                  // println("Getting SootClass for: " + a.getType.toString)
                  val sootcls = Scene.v.getSootClass(a.getType.toString)
                  val applyMethods = sootcls.getMethods.filter(mn => mn.getName.equals("apply")  && !mn.getParameterType(0).toString.equals("java.lang.Object"))
                  // println("number of methods = " + sootcls.getMethodCount)

                  // println("Apply methods found: " + applyMethods.length)
                  for (applyMethod <- applyMethods) {
                    // println("Found method: " + applyMethod.getSignature)
                    worklist += CompileMethodTask(applyMethod.makeRef)
                    applyM = applyMethod
                  }

                }
                // Call(Id(methodName(applyM)), Id("_item_desc"), Id("_arg0"), Id("_this_kernel"))
                // DummyTree("GVirtualInvoke:firepile.Group.items")
                Apply(Select(Ident(LocalValue(NoSymbol,"firepile.Group",PrefixedType(ThisType(Class("firepile")),Class("firepile.Group")))),
      Method(methodName(applyM),MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Any")))), PrefixedType(ThisType(Class("scala")),Class("scala.Any"))))), List(Ident(LocalValue(NoSymbol,"_item_desc",NamedType("firepile.Item"))),Ident(LocalValue(NoSymbol,"_arg0",NamedType("firepile.Group"))), Ident(LocalValue(NoSymbol,"_this_kernel",NamedType("kernel_ENV")))))
            }
            case x => DummyTree("unsupported interface invoke") // Id("unsupported interface invoke:" + v.getClass.getName + " :::::: " + v + " --> " + x)
          }

          case GLocal(name, typ) => v match {

            case GLocal(baseName, SMethodRef(classname, "Something", _, _, _)) => {
              if (anonFuns.contains(name))
                translateExp(anonFuns(name), symtab, anonFuns)
              else
                symtab.addLocalVar(typ, LocalValue(NoSymbol, mangleName(name), translateType(typ))); DummyTree(mangleName(name))
                // symtab.addLocalVar(typ, Id(mangleName(name))); Id(mangleName(name))
            }
            case _ => {
              if (anonFuns != null && anonFuns.contains(name))
                translateExp(anonFuns(name), symtab, anonFuns)
              else
                translateType(typ) match {
                  case NamedType("firepile_Group") => symtab.addLocalVar(typ, LocalValue(NoSymbol, mangleName(name), NamedType("P_firepile_Group"))); Ident(LocalValue(NoSymbol, mangleName(name), translateType(typ)))
                  case NamedType("firepile_Item") => symtab.addLocalVar(typ, LocalValue(NoSymbol, mangleName(name), NamedType("P_firepile_Item"))); Ident(LocalValue(NoSymbol, mangleName(name), translateType(typ)))
                  case _ => symtab.addLocalVar(typ, LocalValue(NoSymbol, mangleName(name), translateType(typ))); Ident(LocalValue(NoSymbol, mangleName(name), translateType(typ)))
                }
                // symtab.addLocalVar(typ, Id(mangleName(name))); Id(mangleName(name))
            }
          }
          case GThisRef(typ) => This(NoSymbol) // DummyTree("_this") // symtab.addThisParam(typ, Id("_this")); Id("_this") }
          //case GThisRef(typ) => { Id("_this") }
          case GParameterRef(typ, index) => { 
            if (!symtab.kernelMethod) { 
              // symtab.addParamVar(typ, index, Id("_arg" + index)); Id("_arg" + index)
               symtab.addParamVar(typ, index, LocalValue(NoSymbol, "_arg" + index, translateType(typ))); Ident(LocalValue(NoSymbol, "_arg" + index, translateType(typ)))
            }
            else DummyTree("_this") // Id("_this")
          }
          // case GParameterRef(typ, index) => { Id("_this") }
          case GStaticFieldRef(fieldRef) => { 
            /* classtab.addClass(new SootClass(fieldRef.`type`.toString));*/ 
            // Id("unimplemented:staticfield") 
            DummyTree("unimplemented:staticfield")
          }
/*
          case GInstanceFieldRef(base: Local, fieldRef) => { /* classtab.addClass(new SootClass(base.getName)); */
            //Select(Deref(base), mangleName(fieldRef.name))
            // println(" mangled Name::" + mangleName(fieldRef.name) + "  original::" + fieldRef.name)
            // Look up instance in environment
            /*
            if (envstructs.contains(ValueType("kernel"), mangleName(fieldRef.name))) {
              Select(Id("_this_kernel"), Id(mangleName(fieldRef.name)))
            }
            else
            */
              DummyTree("GInstanceFieldRef:"+mangleName(fieldRef.name)) // Id(mangleName(fieldRef.name))
          }
*/

          case GInstanceFieldRef(base, fieldRef) => { 
            // println(" base ::" + base + ":::" + fieldRef)

            // Look up instance in environment
            /*
            if (envstructs.contains(ValueType("kernel"), mangleName(fieldRef.name))) {
              Select(Id("_this_kernel"), Id(mangleName(fieldRef.name)))
            }
            else
            */
            // DummyTree("GInstanceFieldRef:"+mangleName(fieldRef.name))  // Id(mangleName(fieldRef.name))
            // Select(Ident(ThisType(Class(translateTypeSimpleName(base.getType)))), Field(mangleName(fieldRef.name), translateType(fieldRef.`type`)))
            // Select(Ident(Class(translateTypeSimpleName(base.getType))), Field(mangleName(fieldRef.name), translateType(fieldRef.`type`)))
            
            // Someday environment struct names may need to be more geeric
            // Select(Ident(Class(translateTypeSimpleName(base.getType))), Field(mangleName(fieldRef.name), translateType(fieldRef.`type`)))
            Select(Ident(Class("_this_kernel")), Field(mangleName(fieldRef.name), translateType(fieldRef.`type`)))
          }

          case GArrayRef(base, index) => Apply(Select(Select(base,Field("data",NoType)),
                                            Method("scala.Array.update",MethodType(List(LocalValue(NoSymbol,"x",translateType(base.getType))),
                                                                       translateType(base.getType)))), List(index))  //ArrayAccess(Select((translateExp(base, symtab, anonFuns)), "data"), translateExp(index, symtab, anonFuns))

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

          case v => DummyTree("translateExp:unsupported:" + v.getClass.getName)

        }
      }
    }
  }

  private def handleIdsVirtualInvoke(v: Value, symtab: SymbolTable, anonFuns: HashMap[String, Value]): Option[Tree] = {
    val fieldName = v.asInstanceOf[soot.jimple.VirtualInvokeExpr].getBase match {
      case ifr: soot.grimp.internal.GInstanceFieldRef => mangleName(ifr.getField.getName)
      case lcl: Local => mangleName(lcl.getName)
    }

    v match {
      // TODO:
      // turn these into field accesses on the struct Id1 passed into the kernel
      // don't assume the local is named "id"
      // handle Id2, Id3, ...r1.

      case GVirtualInvoke(_, SMethodRef(SClassName(_), "barrier", _, _, _), _) => { 
        /* println(" Got barrier here::");*/ 
        // Some(DummyTree("barrier")) //return Some(Call(Id("barrier"), Id("CLK_LOCAL_MEM_FENCE"))) 
       Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Group")))),
                        Method("firepile.Group.barrier",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List()))

      }
      case GVirtualInvoke(base, SMethodRef(SClassName("firepile.Item"), "id", _, _, _), args) => 
        if (args.size > 0) {
          /*
          Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Item")))),
                            Method("firepile.Item.id",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(translateExp(args.head, symtab, anonFuns))))
          */
          Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Item")))),
                            Method("firepile.Item.id",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(translateExp(args.head, symtab, anonFuns))))
        }
        else {
          Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Item")))),
                            Method("firepile.Item.id",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0))))
        }
      case GVirtualInvoke(base, SMethodRef(SClassName("firepile.Item"), "globalId", _, _, _), args) => 
        if (args.size > 0)
          Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Item")))),
                          Method("firepile.Item.globalId",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(translateExp(args.head, symtab, anonFuns))))
      else
        Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Item")))),
                          Method("firepile.Item.globalId",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0))))
    case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Group"), "id", _, _, _), args) =>
      if (args.size > 0) {
        /*
        Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Group")))),
                          Method("firepile.Group.id",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(translateExp(args.head, symtab, anonFuns))))
        */
        Some(Select(Apply(Select(Ident(LocalValue(NoSymbol, "_group_desc", NamedType("firepile_Group"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(translateExp(args.head,symtab,anonFuns)))), Field("id", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))))
      }
      else {
        /*
        Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Group")))),
                          Method("firepile.Group.id",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0))))
        */
        Some(Select(Apply(Select(Ident(LocalValue(NoSymbol, "_group_desc", NamedType("firepile_Group"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0))), Field("id", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))))
      }
    case GVirtualInvoke(_, SMethodRef(SClassName("firepile.Group"), "size", _, _, _), args) => 
      if (args.size > 0)
        Some(Select(Apply(Select(Ident(LocalValue(NoSymbol, "_group_desc", NamedType("firepile_Group"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(translateExp(args.head,symtab,anonFuns)))), Field("size", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))))
      else
        Some(Select(Apply(Select(Ident(LocalValue(NoSymbol, "_group_desc", NamedType("firepile_Group"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0))), Field("size", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))))

    case GVirtualInvoke(base, SMethodRef(SClassName("firepile.Item"), "size", _, _, _), args) => 
      if (args.size > 0)
          Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Item")))),
                            Method("firepile.Item.size",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(translateExp(args.head, symtab, anonFuns))))
        else
          Some(Apply(Select(Ident(LocalValue(NoSymbol,fieldName,PrefixedType(ThisType(Class("firepile")),Class("firepile.Item")))),
                            Method("firepile.Item.size",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))), PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(0))))
      case GVirtualInvoke(base, SMethodRef(SClassName("firepile.util.BufferBackedArray$BBArray"), "update", _, _, _), args) =>
        Some(Apply(Select(Select(translateExp(base, symtab, anonFuns),Field("data",NoType)),
                     Method("firepile.util.BufferBackedArray$BBArray.update",MethodType(List(LocalValue(NoSymbol,"x",translateType(base.getType))),
                                     translateType(base.getType)))), args.map(a => translateExp(a, symtab, anonFuns))))
      case GVirtualInvoke(base, SMethodRef(SClassName("firepile.util.BufferBackedArray$BBArray"), "apply", _, _, _), args) =>
        Some(Apply(Select(Select(translateExp(base, symtab, anonFuns),Field("data",NoType)),
                     Method("firepile.util.BufferBackedArray$BBArray.apply",MethodType(List(LocalValue(NoSymbol,"x",translateType(base.getType))),
                                                                       translateType(base.getType)))), List(translateExp(args.head, symtab, anonFuns))))
      // case GVirtualInvoke(a, method, args) => { println("Generic Virtual Invoke:::" + a + "::" + method + ":::" + args) }
      case _ => None
    }

    //None
  }
  private def translateUnits(units: List[SootUnit], result: List[Tree], symtab: SymbolTable, anonFuns: HashMap[String, Value]): List[Tree] = {
    implicit val iv: (SymbolTable, HashMap[String, Value]) = (symtab, anonFuns)
    units match {
      case u :: us => {
        val tree: Tree = u match {
          case GIdentity(left: Value, right) => {
            left match {
              case l: Local if !(l.getName.equals("this") || l.getName.equals("l0")) => translateType(l.getType) match {
                case NamedType("firepile_Group") => symtab.addLocalVar(l.getType, LocalValue(NoSymbol,mangleName(l.getName),NamedType("P_firepile_Group")))
                case NamedType("firepile_Item") => symtab.addLocalVar(l.getType, LocalValue(NoSymbol,mangleName(l.getName),NamedType("P_firepile_Item")))
                case _ => symtab.addLocalVar(l.getType, LocalValue(NoSymbol,mangleName(l.getName),translateType(l.getType)))
              }
              case _ => { }
            }
            Assign(left,right) 
          }
          case GAssignStmt(left: Local, GNewArray(typ: SootArrayType, size)) => {
            DummyTree("Assign from new array")
          }
          case GAssignStmt(left: Local, right) => {
            envstructs.addStruct(NamedType("kernel"))
            right match {
              // BBArray.ofDim
              case GVirtualInvoke(base, SMethodRef(_, "ofDim", _, _, _), _) => {
                println(" Setting local variable::" + left.getName + "::" + left.getType.toString + "::" + Kernel.blocks)
                Kernel.localArgs.add((left.getName, left.getType, Kernel.blocks))
                val fieldType = translateType(left.getType) match {
                  case ft: NamedType => NamedType("l_" + mangleName(ft.fullname))
                  case x => x
                }
                envstructs.append(NamedType("kernel"), ValDef(LocalValue(NoSymbol, mangleName(left.getName), fieldType), EmptyTree()))
                EmptyTree() 
              }
              // Array.ofDim
              case GCast(GVirtualInvoke(_, method, args), typ) => {
                args.head match {
                  case GInterfaceInvoke(GVirtualInvoke(_, SMethodRef(_, "items", _, _, _), args), SMethodRef(_, "size", _, _, _), _) => {
                    // println(" Setting local variable::" + left.getName + "::" + typ.toString + "::" + Kernel.blocks)
                    Kernel.localArgs.add((left.getName, typ, Kernel.blocks))
                    val fieldType = translateType(typ) match {
                      case ft: NamedType => NamedType("l_" + mangleName(ft.fullname))
                      case x => x
                    }
                    envstructs.append(NamedType("kernel"), ValDef(LocalValue(NoSymbol, mangleName(left.getName), fieldType), EmptyTree()))
                    EmptyTree()
                  }
                  case _ => EmptyTree()
                }
              }
              case _ => Assign(left, right)
            }
          }
          case GAssignStmt(left, right) => Assign(left,right) // DummyTree("GAssignStmt") // Eval(Assign(left, right))
          case GGoto(target) => Goto(translateLabel(target, symtab))
          case GNop() => DummyTree("GNop") // Nop
          case GReturnVoid() => Return(EmptyTree())
          case GReturn(returned) => returned match {
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
                      // println(" Global Variable from inst field ref " + instBase + " :::" + fieldRef.name + ":::" + fieldRef.`type`.toString)
                      Kernel.globalArgs.add((fieldRef.name, fieldRef.`type`,i))
                      envstructs.addStruct(NamedType("kernel"))
                      val fieldType = translateType(fieldRef.`type`, i) match {
                        case ft: NamedType => NamedType("g_" + mangleName(ft.fullname))
                        case x => x
                      }
                      envstructs.append(NamedType("kernel"), ValDef(LocalValue(NoSymbol, mangleName(fieldRef.name), fieldType),EmptyTree()))
                    }
                    case GStaticInvoke(SMethodRef(SClassName(_), name, _, _, _), args) => {
                      for (j <- args) {
                        j match {
                          case GInstanceFieldRef(instBase, fieldRef) => { 
                            // println(" Global Variable from static invoke with instance field ref arg " + instBase + " :::" + fieldRef.name + ":::" + fieldRef.`type`.toString)
                            Kernel.globalArgs.add((fieldRef.name, fieldRef.`type`,i))
                            envstructs.addStruct(NamedType("kernel"))
                            val fieldType = translateType(fieldRef.`type`, i) match {
                              case ft: NamedType => NamedType("g_" + mangleName(ft.fullname))
                              case x => x
                            }
                            envstructs.append(NamedType("kernel"), ValDef(LocalValue(NoSymbol, mangleName(fieldRef.name), fieldType),EmptyTree()))
                          }
                          case _ => {}
                        }
                      }
                    }
                    case _ => {}
                  }
                }
            Return(EmptyTree())
            }

            case _ => Return(translateExp(returned,symtab,anonFuns))
          }

            
            
          case GIf(cond, target) => If(translateExp(cond, symtab, anonFuns), Goto(translateLabel(target, symtab)), EmptyTree())
          case GInvokeStmt(invokeExpr) => translateExp(invokeExpr, symtab, anonFuns) // Eval(translateExp(invokeExpr, symtab, anonFuns))

          // TODO
          case GTableSwitchStmt(key, lowIndex, highIndex, targets, defaultTarget) => DummyTree("GTableSwitchStmt")
          case GLookupSwitchStmt(key: Local, lookupVals: List[Value], targets: List[Stmt], defaultTarget) =>
            DummyTree("GLookupSwitchStmt")
          // IGNORE
          case GThrow(op) => DummyTree("GThrow") // Id("throw unsupported")
          case GExitMonitor(op) => DummyTree("GExitMonitor") // Id("monitors unsupported")
          case GEnterMonitor(op) => DummyTree("GEnterMonitor") // Id("monitors unsupported")
          case GStaticInvoke(method, args) => { /* println("GstaticInvoke::" + method + "::" + args); */ args.map(a => translateExp(a, symtab, anonFuns)).head }

          case _ => { println("huh " + u); DummyTree("unsupported: " + u)  } // Id("unsupported: " + u)
        }

        translateUnits(us, result ::: List[Tree](tree), symtab, anonFuns)
      }
      case Nil => result
    }
  }

  // TODO: pass in anonFuns.  Lookup base in the anonFuns map to get a more precise type.
  private def getPossibleReceivers(base: Value, method: SootMethodRef) = {
    Scene.v.loadDynamicClasses
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

          val classesInPkg = getClassesInPackage(t.getSootClass.getPackageName)

          for (c <- classesInPkg) {
            // println("Class found: " + c.getName + ", adding to Scene")
            // Scene.v.addBasicClass(c.getName, SootClass.HIERARCHY)
            //Scene.v.loadBasicClasses
            //for (m <- Scene.v.loadClass(c.getName, SootClass.HIERARCHY).methodIterator)
            //  println(c.getName + " has method " + m.getName)
            // Scene.v.loadClass(c.getName, SootClass.HIERARCHY)
            
            // Scene.v.addBasicClass(c.getName, SootClass.SIGNATURES)
           
            
            // Scene.v.tryLoadClass(c.getName, SootClass.HIERARCHY)
            // Scene.v.tryLoadClass(c.getName, SootClass.SIGNATURES)
            // Scene.v.tryLoadClass(c.getName, SootClass.BODIES)
            Scene.v.forceResolve(c.getName, SootClass.BODIES) 
            // soot.SootResolver.v.resolveClass(c.getName, SootClass.SIGNATURES) 
          }

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

            // queue ++= H.getDirectSubclassesOf(c).asInstanceOf[java.util.List[SootClass]]toList
            // println("Getting subclasses of " + c.getName + " through package name " + c.getPackageName)


            queue ++= H.getSubclassesOf(c).asInstanceOf[java.util.List[SootClass]].toList

            /*
            for (i <- queue)
              println("Subclass found: " + i)
            */
          }

          if (result.isEmpty)
            method.declaringClass :: Nil
          else
            result.distinct.toList
        }

        case _ => Nil
      }
    }
  }

  private def insertLabels(units: List[SootUnit], symtab: SymbolTable, result: List[Tree], resultWithLabels: List[Tree]): List[Tree] = units match {
    case u :: us => {
      symtab.labels.get(u) match {
        case Some(label) => insertLabels(us, symtab, result.tail, resultWithLabels ::: Target(LabelSymbol(label), EmptyTree()) :: result.head :: Nil)
        case None => insertLabels(us, symtab, result.tail, resultWithLabels ::: result.head :: Nil)
      }
    }
    case Nil => resultWithLabels
  }

  private val arraystructs = new ArrayStructs()
  private val envstructs = new EnvStructs()

  private def translateLabel(u: SootUnit, symtab: SymbolTable): LabelSymbol = u match {
    case target: Stmt => {
      symtab.labels.get(target) match {
        case Some(label) => LabelSymbol(label)
        case None => {
          val label = symtab.nextLabel
          symtab.labels += target -> label
          LabelSymbol(label)
        }
      }
    }
    case _ => LabelSymbol("Label")
  }

  private def makeFunction(m: SootMethod, body: List[Tree], symtab: SymbolTable, takesThis: Boolean) = {
    //     val params = new HashMap[Int, (Symbol, Type)]() 
    val varTree = new ListBuffer[Tree]()

    val funParams = new ListBuffer[Symbol]()
    for (i <- 0 until symtab.params.size) {
      symtab.params.get(i) match {
        case Some((sym, typ)) => funParams += sym
        case None => throw new RuntimeException("Missing param building Function")
      }
    }

    funParams.headOption match {
      case Some(LocalValue(_, _, NamedType("firepile_Group"))) => {
        funParams += LocalValue(NoSymbol, "_this_kernel", NamedType("kernel_ENV"))
        varTree += ArrayDef(LocalValue(NoSymbol, "_item_desc", NamedType("firepile_Item")), Literal(kernelDim))
        
        for (i <- 0 until kernelDim) {
          varTree += Assign(
                      Select(Apply(Select(Ident(LocalValue(NoSymbol, "_item_desc", NamedType("firepile_Item"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))), Field("id", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                      Apply(Select(Ident(NoSymbol), Method("get_local_id",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                                                                       PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))))

          varTree += Assign(
                      Select(Apply(Select(Ident(LocalValue(NoSymbol, "_item_desc", NamedType("firepile_Item"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))), Field("size", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                      Apply(Select(Ident(NoSymbol), Method("get_local_size",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                                                                       PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))))
          varTree += Assign(
                      Select(Apply(Select(Ident(LocalValue(NoSymbol, "_item_desc", NamedType("firepile_Item"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))), Field("globalId", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                      Apply(Select(Ident(NoSymbol), Method("get_global_id",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                                                                       PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))))
          /*
          varTree += Assign(Select(ArrayAccess(Id("_item_desc"), IntLit(i)), Id("id")), Call(Id("get_local_id"), IntLit(i)))
          varTree += Assign(Select(ArrayAccess(Id("_item_desc"), IntLit(i)), Id("size")), Call(Id("get_local_size"), IntLit(i)))
          varTree += Assign(Select(ArrayAccess(Id("_item_desc"), IntLit(i)), Id("globalId")), Call(Id("get_global_id"), IntLit(i)))
          */
        }

      
      }
      case Some(LocalValue(_, _, NamedType("firepile_Item"))) => {
        // println("THIS IS THE Item Method")
        funParams += LocalValue(NoSymbol, "_group_desc", NamedType("firepile_Group"))
        funParams += LocalValue(NoSymbol, "_this_kernel", NamedType("kernel_ENV"))
      }
      case None => {}
      case _ => {}
    }

    if (symtab.kernelMethod) { 
      for (i <- Kernel.globalArgs) {
        val (t: Type, s: String) = translateType("global", i._2, i._1, i._3)
        funParams += LocalValue(NoSymbol, s, t)
      }

      for (i <- Kernel.localArgs) {
        val (t: Type, s: String) = translateType("local", i._2, i._1, -1)
        funParams += LocalValue(NoSymbol, s, t)
      }
      
      println("#### globalArgs = " + Kernel.globalArgs.length)
      println("#### localArgs = " + Kernel.localArgs.length)
    }

    if (symtab.kernelMethod) {
      varTree += ArrayDef(LocalValue(NoSymbol, "_group_desc", NamedType("firepile_Group")), Literal(kernelDim))
      for (i <- 0 until kernelDim) {
          varTree += Assign(
                      Select(Apply(Select(Ident(LocalValue(NoSymbol, "_group_desc", NamedType("firepile_Group"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))), Field("id", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                      Apply(Select(Ident(NoSymbol), Method("get_group_id",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                                                                       PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))))
          varTree += Assign(
                      Select(Apply(Select(Ident(LocalValue(NoSymbol, "_group_desc", NamedType("firepile_Group"))), Method("apply", MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))), Field("size", PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                      Apply(Select(Ident(NoSymbol), Method("get_global_size",MethodType(List(LocalValue(NoSymbol,"x",PrefixedType(ThisType(Class("scala")),Class("scala.Int")))),
                                                                       PrefixedType(ThisType(Class("scala")),Class("scala.Int"))))), List(Literal(i))))
      }
    }

    Function(funParams.toList, Block(symtab.locals.toList ::: varTree.toList ::: body, Ident(Method(methodName(m),translateType(m.getReturnType)))))
  }
  

  // Adapted from http://snippets.dzone.com/posts/show/4831
  def getClassesInPackage(pkgName: String): List[java.lang.Class[_]] = {
    val classLoader = Thread.currentThread.getContextClassLoader

    val path = pkgName.replace('.', '/')
    val resources = classLoader.getResources(path)

    val dirs = new ListBuffer[java.io.File]()

    for (r <- resources) {
      val fileName = r.getFile
      val fileNameDecoded = java.net.URLDecoder.decode(fileName, "UTF-8")
      dirs += new java.io.File(fileNameDecoded)
    }

    val classes = new ListBuffer[java.lang.Class[_]]()

    for (d <- dirs)
      classes ++= findClasses(d, pkgName)

    classes.toList
  }

  def findClasses(directory: java.io.File, pkgName: String): ListBuffer[java.lang.Class[_]] = {
    val classes = new ListBuffer[java.lang.Class[_]]()

    if (!directory.exists)
      return classes

    val files = directory.listFiles

    for (file <- files) {
      val fileName = file.getName
      if (file.isDirectory)
        classes ++= findClasses(file, pkgName + "." + fileName)
      else if (fileName.endsWith(".class") && !fileName.contains("$")) {
        var _class: java.lang.Class[_] = null

        try {
          _class = java.lang.Class.forName(pkgName + "." + fileName.substring(0, fileName.length-6))
        }
        catch {
          case e: ExceptionInInitializerError => {
       	  _class = java.lang.Class.forName(pkgName + '.' + fileName.substring(0, fileName.length-6),
            false, Thread.currentThread.getContextClassLoader)
          }
        }
        classes += _class
      }
    }

    classes
  }
}
