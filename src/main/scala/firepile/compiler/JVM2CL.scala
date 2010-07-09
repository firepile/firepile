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
      println("usage: sooty.Main className methodSig")
      exit(1)
    }

    val className = args(0)
    val methodSig = args(1)

    compileRoot(className, methodSig)
  }

  private val makeCallGraph = false
  private val HACK = false

  setup

  def compileRoot(className: String, methodSig: String, self: AnyRef = null): List[Tree] = {
    println("compiling " + className + "." + methodSig)
    try {
      if (HACK) addRootMethodToWorklist("scala.collection.immutable.List", "dropWhile(Lscala/Function1;)Ljava/lang/Object;", null) else
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
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0-local/classes"
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0-local/test-classes"
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0.RC3/classes"
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0.RC3/test-classes"
      + ":/Users/nystrom/firepile/target/scala_2.8.0.RC3/classes"
      + ":/Users/nystrom/firepile/target/scala_2.8.0.RC3/test-classes"
      + ":/Users/dwhite/svn/firepile/target/scala_2.8.0.RC3/classes"
      + ":/Users/dwhite/svn/firepile/target/scala_2.8.0.RC3/test-classes"
      + ":.:tests:bin:lib/soot-2.4.0.jar:/opt/local/share/scala-2.8/lib/scala-library.jar")
    
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
    def method: SootMethod
  }

  def mapDef(m: SootMethod, t: Tree) = t
  def reduceDef(m: SootMethod, t: Tree) = t

  object CompileMethodTask {
    def apply(m: SootMethodRef, self: AnyRef = null): CompileMethodTask = CompileMethodTask(m.resolve, self)
  }
  
  case class CompileMethodTask(method: SootMethod, self: AnyRef) extends Task {
    def run = compileMethod(method, self) match {
      case null => Nil
      case t => t::Nil
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
    Scene.v.setEntryPoints(worklist.toList.map(p => p.method))
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

  implicit def cf2cf(cf: Classfile) = new Cf(cf)

  class Cf(val cf: Classfile) {
    def getName(n: Int): String = {
      import cf.pool._

      cf.pool(n) match {
        case UTF8(str) => str
        case StringConst(m) => getName(m)
        case ClassRef(m) => getName(m)
        case _ => "<error>"
      }
    }

    def getType(n: Int): String = getName(n)


  def getFormals(str: String): List[Typ] = sigToType(str) match {
    case MTyp(_, ts, _) => ts
  }
  def getReturn(str: String): Typ = sigToType(str) match {
    case MTyp(_, _, t) => t
  }

  def nameToClass(str: String) = str.replace('/', '.')

  def sigToType(str: String): Typ = sigToType(str, 0)._1

  def sigToType(str: String, i: Int): (Typ, Int) = str.charAt(i) match {
    case 'B' => (NamedTyp("scala.Byte"), i+1)
    case 'S' => (NamedTyp("scala.Short"), i+1)
    case 'C' => (NamedTyp("scala.Char"), i+1)
    case 'Z' => (NamedTyp("scala.Boolean"), i+1)
    case 'I' => (NamedTyp("scala.Int"), i+1)
    case 'J' => (NamedTyp("scala.Long"), i+1)
    case 'F' => (NamedTyp("scala.Float"), i+1)
    case 'D' => (NamedTyp("scala.Double"), i+1)
    case 'V' => (NamedTyp("scala.Unit"), i+1)
    case 'L' =>
      val j = str.indexOf(';', i)
      (NamedTyp(nameToClass(str.substring(i + 1, j))), j + 1)
    case '[' =>
      val (tpe, j) = sigToType(str, i + 1)
      (ParamTyp(NamedTyp("scala.Array"), tpe :: Nil), j)
    case '(' =>
      val (tpes, tpe, j) = sigToType0(str, i + 1)
      (MTyp(Nil, tpes, tpe), j)
  }

  def sigToType0(str: String, i: Int): (List[Typ], Typ, Int) =
    if (str.charAt(i) == ')') {
      val (tpe, j) = sigToType(str, i+1)
      (Nil, tpe, j)
    }
    else {
      val (tpe, j) = sigToType(str, i)
      val (rest, ret, k) = sigToType0(str, j)
      (tpe :: rest, ret, k)
    }

  def getSig(flags: Int, name: Int, tpe: Int, attribs: List[cf.Attribute]) : Sig = {
    attribs find {
      case cf.Attribute(name, _) => getName(name) == "JacoMeta"
    } match {
      case Some(cf.Attribute(_, data)) =>
        val mp = new MetaParser(getName(
          ((data(0) & 0xff) << 8) + (data(1) & 0xff)).trim())
        mp.parse match {
          case None =>
            if (getName(name) == "<init>") {
              Sig("this", Nil, getFormals(getType(tpe)), getReturn(getType(tpe)))
            } else {
              Sig(Names.decode(getName(name)), Nil, getFormals(getType(tpe)), getReturn(getType(tpe)))
            }
          case Some(str) =>
            if (getName(name) == "<init>")
              Sig("this", Nil, getFormals(str), getReturn(str))
            else
              Sig(Names.decode(getName(name)), Nil, getFormals(str), getReturn(str))
        }
      case None =>
        if (getName(name) == "<init>") {
          Sig("this", Nil, getFormals(getType(tpe)), getReturn(getType(tpe)))
        } else {
          Sig(Names.decode(getName(name)), Nil, getFormals(getType(tpe)), getReturn(getType(tpe)))
        }
    }
  }

  }

  def parseScalaSignature(scalaSig: ScalaSig, isPackageObject: Boolean) = {
    import java.io.{PrintStream, OutputStreamWriter, ByteArrayOutputStream}

    val baos = new ByteArrayOutputStream
    val stream = new PrintStream(baos)
    val syms = scalaSig.topLevelClasses ::: scalaSig.topLevelObjects
    syms.head.parent match {
    //Partial match
      case Some(p) if (p.name != "<empty>") => {
        val path = p.path
        if (!isPackageObject) {
          stream.print("package ");
          stream.print(path);
          stream.print("\n")
        } else {
          val i = path.lastIndexOf(".")
          if (i > 0) {
            stream.print("package ");
            stream.print(path.substring(0, i))
            stream.print("\n")
          }
        }
      }
      case _ =>
    }
    // Print classes
    val printer = new ScalaSigPrinter(stream, false)
    for (c <- syms) {
      printer.printSymbol(c)
    }
    baos.toString
  }

class ScalaSigPrinter(stream: java.io.PrintStream, printPrivates: Boolean) {
  import java.io.{PrintStream, ByteArrayOutputStream}
  import java.util.regex.Pattern

  import scala.tools.scalap.scalax.util.StringUtil
  import reflect.NameTransformer
  import java.lang.String

  import stream._

  val CONSTRUCTOR_NAME = "<init>"

  case class TypeFlags(printRep: Boolean)

  def printSymbol(symbol: Symbol) {printSymbol(0, symbol)}

  def printSymbolAttributes(s: Symbol, onNewLine: Boolean, indent: => Unit) = s match {
    case t: SymbolInfoSymbol => {
      for (a <- t.attributes) {
        indent; print(toString(a))
        if (onNewLine) print("\n") else print(" ")
      }
    }
    case _ =>
  }

  def printSymbol(level: Int, symbol: Symbol) {
    if (!symbol.isLocal &&
            !(symbol.isPrivate && !printPrivates)) {
      def indent() {for (i <- 1 to level) print("  ")}

      printSymbolAttributes(symbol, true, indent)
      symbol match {
        case o: ObjectSymbol =>
          if (!isCaseClassObject(o)) {
            indent
            if (o.name == "package") {
              // print package object
              printPackageObject(level, o)
            } else {
              printObject(level, o)
            }
          }
        case c: ClassSymbol if !refinementClass(c) && !c.isModule =>
          indent
          printClass(level, c)
        case m: MethodSymbol =>
          printMethod(level, m, indent)
        case a: AliasSymbol =>
          indent
          printAlias(level, a)
        case t: TypeSymbol if !t.isParam && !t.name.matches("_\\$\\d+")=>
          indent
          printTypeSymbol(level, t)
        case s =>
      }
    }
  }

  def isCaseClassObject(o: ObjectSymbol): Boolean = {
    val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
    o.isFinal && (classSymbol.children.find(x => x.isCase && x.isInstanceOf[MethodSymbol]) match {
      case Some(_) => true
      case None => false
    })
  }

  private def underCaseClass(m: MethodSymbol) = m.parent match {
    case Some(c: ClassSymbol) => c.isCase
    case _ => false
  }


  private def printChildren(level: Int, symbol: Symbol) {
    for (child <- symbol.children) printSymbol(level + 1, child)
  }

  def printWithIndent(level: Int, s: String) {
    def indent() {for (i <- 1 to level) print("  ")}
    indent;
    print(s)
  }

  def printModifiers(symbol: Symbol) {
    // print private access modifier
    if (symbol.isPrivate) print("private ")
    else if (symbol.isProtected) print("protected ")
    else symbol match {
      case sym: SymbolInfoSymbol => sym.symbolInfo.privateWithin match {
        case Some(t: Symbol) => print("private[" + t.name +"] ")
        case _ =>
      }
      case _ =>
    }

    if (symbol.isSealed) print("sealed ")
    if (symbol.isImplicit) print("implicit ")
    if (symbol.isFinal && !symbol.isInstanceOf[ObjectSymbol]) print("final ")
    if (symbol.isOverride) print("override ")
    if (symbol.isAbstract) symbol match {
      case c@(_: ClassSymbol | _: ObjectSymbol) if !c.isTrait => print("abstract ")
      case _ => ()
    }
    if (symbol.isCase && !symbol.isMethod) print("case ")
  }

  private def refinementClass(c: ClassSymbol) = c.name == "<refinement>"

  def printClass(level: Int, c: ClassSymbol) {
    if (c.name == "<local child>" /*scala.tools.nsc.symtab.StdNames.LOCALCHILD.toString()*/ ) {
      print("\n")
    } else {
      printModifiers(c)
      val defaultConstructor = if (c.isCase) getPrinterByConstructor(c) else ""
      if (c.isTrait) print("trait ") else print("class ")
      print(processName(c.name))
      val it = c.infoType
      val classType = it match {
        case PolyType(typeRef, symbols) => PolyTypeWithCons(typeRef, symbols, defaultConstructor)
        case ClassInfoType(a, b) if c.isCase => ClassInfoTypeWithCons(a, b, defaultConstructor)
        case _ => it
      }
      printType(classType)
      print(" {")
      //Print class selftype
      c.selfType match {
        case Some(t: Type) => print("\n"); print(" this : " + toString(t) + " =>")
        case None =>
      }
      print("\n")
      printChildren(level, c)
      printWithIndent(level, "}\n")
    }
  }

  def getPrinterByConstructor(c: ClassSymbol) = {
    c.children.find {
      case m: MethodSymbol if m.name == CONSTRUCTOR_NAME => true
      case _ => false
    } match {
      case Some(m: MethodSymbol) =>
        val baos = new ByteArrayOutputStream
        val stream = new PrintStream(baos)
        val printer = new ScalaSigPrinter(stream, printPrivates)
        printer.printMethodType(m.infoType, false)(())
        baos.toString
      case None =>
        ""
    }
  }

  def printPackageObject(level: Int, o: ObjectSymbol) {
    printModifiers(o)
    print("package ")
    print("object ")
    val poName = o.symbolInfo.owner.name
    print(processName(poName))
    val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
    printType(classSymbol)
    print(" {\n")
    printChildren(level, classSymbol)
    printWithIndent(level, "}\n")

  }

  def printObject(level: Int, o: ObjectSymbol) {
    printModifiers(o)
    print("object ")
    print(processName(o.name))
    val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
    printType(classSymbol)
    print(" {\n")
    printChildren(level, classSymbol)
    printWithIndent(level, "}\n")
  }

  def genParamNames(t: {def paramTypes: Seq[Type]}): List[String] = t.paramTypes.toList.map(x => {
    var str = toString(x)
    val j = str.indexOf("[")
    if (j > 0) str = str.substring(0, j)
    str = StringUtil.trimStart(str, "=> ")
    var i = str.lastIndexOf(".")
    val res = if (i > 0) str.substring(i + 1) else str
    if (res.length > 1) StringUtil.decapitalize(res.substring(0, 1)) else res.toLowerCase
  })

  implicit object _tf extends TypeFlags(false)

  def printMethodType(t: Type, printResult: Boolean)(cont: => Unit): Unit = {

    def _pmt(mt: Type {def resultType: Type; def paramSymbols: Seq[Symbol]}) = {

      val paramEntries = mt.paramSymbols.map({
        case ms: MethodSymbol => ms.name + " : " + toString(ms.infoType)(TypeFlags(true))
        case _ => "^___^"
      })

      // Print parameter clauses
      print(paramEntries.mkString(
        "(" + (mt match {case _: ImplicitMethodType => "implicit "; case _ => ""})
        , ", ", ")"))

      // Print result type
      mt.resultType match {
        case mt: MethodType => printMethodType(mt, printResult)({})
        case imt: ImplicitMethodType => printMethodType(imt, printResult)({})
        case x => if (printResult) {
          print(" : ");
          printType(x)
        }
      }
    }

    t match {
      case mt@MethodType(resType, paramSymbols) => _pmt(mt)
      case mt@ImplicitMethodType(resType, paramSymbols) => _pmt(mt)
      case pt@PolyType(mt, typeParams) => {
        print(typeParamString(typeParams))
        printMethodType(mt, printResult)({})
      }
      //todo consider another method types
      case x => print(" : "); printType(x)
    }

    // Print rest of the symbol output
    cont
  }

  def printMethod(level: Int, m: MethodSymbol, indent: () => Unit) {
    def cont = print(" = { /* compiled code */ }")

    val n = m.name
    if (underCaseClass(m) && n == CONSTRUCTOR_NAME) return
    if (n.matches(".+\\$default\\$\\d+")) return // skip default function parameters
    if (n.startsWith("super$")) return // do not print auxiliary qualified super accessors
    if (m.isAccessor && n.endsWith("_$eq")) return
    indent()
    printModifiers(m)
    if (m.isAccessor) {
      val indexOfSetter = m.parent.get.children.indexWhere(x => x.isInstanceOf[MethodSymbol] &&
              x.asInstanceOf[MethodSymbol].name == n + "_$eq")
      print(if (indexOfSetter > 0) "var " else "val ")
    } else {
      print("def ")
    }
    n match {
      case CONSTRUCTOR_NAME =>
        print("this")
        printMethodType(m.infoType, false)(cont)
      case name =>
        val nn = processName(name)
        print(nn)
        printMethodType(m.infoType, true)(
          {if (!m.isDeferred) print(" = { /* compiled code */ }" /* Print body only for non-abstract methods */ )}
          )
    }
    print("\n")
  }

  def printAlias(level: Int, a: AliasSymbol) {
    print("type ")
    print(processName(a.name))
    printType(a.infoType, " = ")
    print("\n")
    printChildren(level, a)
  }

  def printTypeSymbol(level: Int, t: TypeSymbol) {
    print("type ")
    print(processName(t.name))
    printType(t.infoType)
    print("\n")
  }

  def toString(attrib: AttributeInfo): String  = {
    val buffer = new StringBuffer
    buffer.append(toString(attrib.typeRef, "@"))
    if (attrib.value.isDefined) {
      buffer.append("(")
      val value = attrib.value.get
      val stringVal = value.isInstanceOf[String]
      if (stringVal) buffer.append("\"")
      val stringValue = valueToString(value)
      val isMultiline = stringVal && (stringValue.contains("\n")
              || stringValue.contains("\r"))
      if (isMultiline) buffer.append("\"\"")
      buffer.append(valueToString(value))
      if (isMultiline) buffer.append("\"\"")
      if (stringVal) buffer.append("\"")
      buffer.append(")")
    }
    if (!attrib.values.isEmpty) {
      buffer.append(" {")
      for (p <- attrib.values) {
        val name = p._1
        val value = p._2
        buffer.append(" val ")
        buffer.append(processName(name))
        buffer.append(" = ")
        buffer.append(valueToString(value))
      }
      buffer.append(valueToString(attrib.value))
      buffer.append(" }")
    }
    buffer.toString
  }

  def valueToString(value: Any): String = value match {
    case t: Type => toString(t)
    // TODO string, char, float, etc.
    case _ => value.toString
  }

  def printType(sym: SymbolInfoSymbol)(implicit flags: TypeFlags): Unit = printType(sym.infoType)(flags)

  def printType(t: Type)(implicit flags: TypeFlags): Unit = print(toString(t)(flags))

  def printType(t: Type, sep: String)(implicit flags: TypeFlags): Unit = print(toString(t, sep)(flags))

  def toString(t: Type)(implicit flags: TypeFlags): String = toString(t, "")(flags)

  def toString(t: Type, sep: String)(implicit flags: TypeFlags): String = {
    // print type itself
    t match {
      case ThisType(symbol) => sep + processName(symbol.path) + ".type"
      case SingleType(typeRef, symbol) => sep + processName(symbol.path) + ".type"
      case ConstantType(constant) => sep + (constant match {
        case null => "scala.Null"
        case _: Unit => "scala.Unit"
        case _: Boolean => "scala.Boolean"
        case _: Byte => "scala.Byte"
        case _: Char => "scala.Char"
        case _: Short => "scala.Short"
        case _: Int => "scala.Int"
        case _: Long => "scala.Long"
        case _: Float => "scala.Float"
        case _: Double => "scala.Double"
        case _: String => "java.lang.String"
        case c: Class[_] => "java.lang.Class[" + c.getComponentType.getCanonicalName.replace("$", ".") + "]"
      })
      case TypeRefType(prefix, symbol, typeArgs) => sep + (symbol.path match {
        case "scala.<repeated>" => flags match {
          case TypeFlags(true) => toString(typeArgs.head) + "*"
          case _ => "scala.Seq" + typeArgString(typeArgs)
        }
        case "scala.<byname>" => "=> " + toString(typeArgs.head)
        case _ => {
          val path = StringUtil.cutSubstring(symbol.path)(".package") //remove package object reference
          StringUtil.trimStart(processName(path) + typeArgString(typeArgs), "<empty>.")
        }
      })
      case TypeBoundsType(lower, upper) => {
        val lb = toString(lower)
        val ub = toString(upper)
        val lbs = if (!lb.equals("scala.Nothing")) " >: " + lb else ""
        val ubs = if (!ub.equals("scala.Any")) " <: " + ub else ""
        lbs + ubs
      }
      case RefinedType(classSym, typeRefs) => sep + typeRefs.map(toString).mkString("", " with ", "")
      case ClassInfoType(symbol, typeRefs) => sep + typeRefs.map(toString).mkString(" extends ", " with ", "")
      case ClassInfoTypeWithCons(symbol, typeRefs, cons) => sep + typeRefs.map(toString).
              mkString(cons + " extends ", " with ", "")

      case ImplicitMethodType(resultType, _) => toString(resultType, sep)
      case MethodType(resultType, _) => toString(resultType, sep)

      case PolyType(typeRef, symbols) => typeParamString(symbols) + toString(typeRef, sep)
      case PolyTypeWithCons(typeRef, symbols, cons) => typeParamString(symbols) + processName(cons) + toString(typeRef, sep)
      case AnnotatedType(typeRef, attribTreeRefs) => {
        toString(typeRef, sep)
      }
      case AnnotatedWithSelfType(typeRef, symbol, attribTreeRefs) => toString(typeRef, sep)
      //case DeBruijnIndexType(typeLevel, typeIndex) =>
      case ExistentialType(typeRef, symbols) => {
        val refs = symbols.map(toString _).filter(!_.startsWith("_")).map("type " + _)
        toString(typeRef, sep) + (if (refs.size > 0) refs.mkString(" forSome {", "; ", "}") else "")
      }
      case _ => sep + t.toString
    }
  }

  def getVariance(t: TypeSymbol) = if (t.isCovariant) "+" else if (t.isContravariant) "-" else ""

  def toString(symbol: Symbol): String = symbol match {
    case symbol: TypeSymbol => {
      val attrs = (for (a <- symbol.attributes) yield toString(a)).mkString(" ")
      val atrs = if (attrs.length > 0) attrs.trim + " " else ""
      atrs + getVariance(symbol) + processName(symbol.name) + toString(symbol.infoType)
    }
    case s => symbol.toString
  }

  def typeArgString(typeArgs: Seq[Type]): String =
    if (typeArgs.isEmpty) ""
    else typeArgs.map(toString).map(StringUtil.trimStart(_, "=> ")).mkString("[", ", ", "]")

  def typeParamString(params: Seq[Symbol]): String =
    if (params.isEmpty) ""
    else params.map(toString).mkString("[", ", ", "]")

  val _syms = Map("\\$bar" -> "|", "\\$tilde" -> "~",
    "\\$bang" -> "!", "\\$up" -> "^", "\\$plus" -> "+",
    "\\$minus" -> "-", "\\$eq" -> "=", "\\$less" -> "<",
    "\\$times" -> "*", "\\$div" -> "/", "\\$bslash" -> "\\\\",
    "\\$greater" -> ">", "\\$qmark" -> "?", "\\$percent" -> "%",
    "\\$amp" -> "&", "\\$colon" -> ":", "\\$u2192" -> "→",
    "\\$hash" -> "#")
  val pattern = Pattern.compile(_syms.keys.foldLeft("")((x, y) => if (x == "") y else x + "|" + y))
  val placeholderPattern = "_\\$(\\d)+"

  private def stripPrivatePrefix(name: String) = {
    val i = name.lastIndexOf("$$")
    if (i > 0) name.substring(i + 2) else name
  }

  def processName(name: String) = {
    val stripped = stripPrivatePrefix(name)
    val m = pattern.matcher(stripped)
    var temp = stripped
    while (m.find) {
      val key = m.group
      val re = "\\" + key
      temp = temp.replaceAll(re, _syms(re))
    }
    val result = temp.replaceAll(placeholderPattern, "_")
    NameTransformer.decode(result)
  }

}

  def unpickleFromAnnotation(classFile: ClassFile, isPackageObject: Boolean): String = {
    val SCALA_SIG_ANNOTATION = "Lscala/reflect/ScalaSignature;"
    val BYTES_VALUE = "bytes"
    import classFile._
    import scalax.rules.scalasig.ClassFileParser.{ConstValueIndex, Annotation}
    import scala.reflect.generic.ByteCodecs
    classFile.annotation(SCALA_SIG_ANNOTATION) match {
      case None => ""
      case Some(Annotation(_, elements)) =>
        val bytesElem = elements.find(elem => constant(elem.elementNameIndex) == BYTES_VALUE).get
        val bytes = ((bytesElem.elementValue match {case ConstValueIndex(index) => constantWrapped(index)})
                .asInstanceOf[StringBytesPair].bytes)
        val length = ByteCodecs.decode(bytes)
        val scalaSig = ScalaSigAttributeParsers.parse(ByteCode(bytes.take(length)))
        parseScalaSignature(scalaSig, isPackageObject)
    }
  }


      case class Sig(name: String, typ: MTyp)
      object Sig {
def apply(name: String, typeFormals: List[Param], formals: List[Typ], returnType: Typ): Sig = Sig(name, MTyp(typeFormals, formals, returnType))
      }

      sealed class Typ
      case class MTyp(typeFormals: List[Param], formals: List[Typ], returnType: Typ) extends Typ
      case class Param(name: String)
      case class NamedTyp(name: String) extends Typ
      case class ParamTyp(base: Typ, args: List[Typ]) extends Typ


  private def compileMethod(m: SootMethod, self: AnyRef): Tree = {
    println("-------------------------------------------------------")
    println(m)

    if (m.isAbstract)
        return null
    if (m.isNative)
        return null

    symtab = new SymbolTable(self)

    if (HACK) {
      val name = m.getDeclaringClass.getName
      val cl = java.lang.Class.forName(name).getClassLoader
      val is = (if (cl == null) java.lang.ClassLoader.getSystemClassLoader else cl).getResourceAsStream(name.replace('.', '/') + ".class")
      val bis = new java.io.ByteArrayOutputStream
      while (is.available > 0)
        bis.write(is.read)
val bytes = bis.toByteArray
      val reader = new ByteArrayReader(bytes)
      val cf = new Classfile(reader)

      println("printing " + cf)
      val sigs = 
      cf.methods flatMap {
        case z@cf.Member(_, flags, name, tpe, attribs) if cf.getName(name).equals(m.getName) =>
          val w = new Cf(cf)
          Some(w.getSig(flags, name, tpe, attribs.asInstanceOf[List[w.cf.Attribute]]))
        case _ => None
      }

      println(sigs)


val classname = name
val encName = Names.encode(if (classname == "scala.AnyRef") "java.lang.Object" else classname)

val isPackageObject = Scalap.isPackageObjectFile(encName)

    val classFile = ClassFileParser.parse(ByteCode(bytes))

  val SCALA_SIG = "ScalaSig"

      println("printing scalasig")

      val sig =
    classFile.attribute(SCALA_SIG).map(_.byteCode).map(ScalaSigAttributeParsers.parse) match {
      // No entries in ScalaSig attribute implies that the signature is stored in the annotation
      case Some(ScalaSig(_, _, entries)) if entries.length == 0 => unpickleFromAnnotation(classFile, isPackageObject)
      case Some(scalaSig) => Scalap.parseScalaSignature(scalaSig, isPackageObject)
      case None => ""
    }

    println(sig)
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

    /*
    if (! m.isStatic) {
      assert(symtab.thisParam != null)
      val (id,typ) = symtab.thisParam
      paramTree += Formal(translateType(typ), id)
    }
    */

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
