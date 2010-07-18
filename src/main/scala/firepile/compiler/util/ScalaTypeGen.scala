package firepile.compiler.util

import java.util.StringTokenizer

import scala.tools.scalap._
import scala.tools.scalap.{Main => Scalap}
import scala.tools.scalap.scalax.rules.scalasig._

import soot.{Type => SootType}

import java.util.ArrayList
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import scala.Seq
import scala.collection.mutable.HashMap

object ScalaTypeGen {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("usage: ScalaTypeGen classname")
      exit(1)
    }
getScalaSignature(args(0))
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
    else
      buffer.append("/*deferred*/ ")
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

    def getFormals(str: String): List[ScalaType] = sigToType(str) match {
      case MTyp(_, ts, _) => ts
    }
    def getReturn(str: String): ScalaType = sigToType(str) match {
      case MTyp(_, _, t) => t
    }

    def nameToClass(str: String) = str.replace('/', '.')

    def sigToType(str: String): ScalaType = sigToType(str, 0)._1

    def sigToType(str: String, i: Int): (ScalaType, Int) = str.charAt(i) match {
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
      (InstTyp(NamedTyp("scala.Array"), tpe :: Nil), j)
      case '(' =>
      val (tpes, tpe, j) = sigToType0(str, i + 1)
      (MTyp(Nil, tpes, tpe), j)
    }

    def sigToType0(str: String, i: Int): (List[ScalaType], ScalaType, Int) =
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
          }
          else {
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
        }
        else {
          Sig(Names.decode(getName(name)), Nil, getFormals(getType(tpe)), getReturn(getType(tpe)))
        }
      }
    }
  }

  def parseScalaSignature(scalaSig: ScalaSig, isPackageObject: Boolean): List[MyClassDef] = {
    val syms = scalaSig.topLevelClasses ::: scalaSig.topLevelObjects
    val pkgPath = syms.head.parent match {
      //Partial match
      case Some(p) if (p.name != "<empty>") => {
        val path = p.path
        if (!isPackageObject) {
          path
        }
        else {
          val i = path.lastIndexOf(".")
          if (i > 0) path.substring(0, i) else ""
        }
      }
      case _ => ""
    }

    val m = new ClassDefMaker(pkgPath)
    syms flatMap (m.makeClassDef _)
  }

  class ClassDefMaker(pkg: String) {
    import java.io.{PrintStream, ByteArrayOutputStream}
    import java.util.regex.Pattern

    import scala.tools.scalap.scalax.util.StringUtil
    import reflect.NameTransformer
    import java.lang.String

    val baos = new ByteArrayOutputStream
    val stream = new PrintStream(baos)

    import stream._

    val CONSTRUCTOR_NAME = "<init>"

    case class TypeFlags(printRep: Boolean)

    def makeClassDef(symbol: Symbol) = makeSymbol(0, symbol) match {
      case Some(x: MyClassDef) => Some(x)
      case _ => None
    }

    //added

    def printSymbolAttributes(s: Symbol, onNewLine: Boolean, indent: => Unit) = s match {
      case t: SymbolInfoSymbol => {
        for (a <- t.attributes) {
          indent; print(toString(a))
          if (onNewLine) print("\n") else print(" ")
        }
      }
      case _ =>
    }

    def makeSymbol(level: Int, symbol: Symbol): Option[MySymbol] = {
      if (!symbol.isLocal) {
        // TODO
        // printSymbolAttributes(symbol, true, indent)
        symbol match {
          case o: ObjectSymbol =>
            if (!isCaseClassObject(o)) {
              if (o.name == "package") {
                // print package object
                Some(makePackageObject(level, o))
              }
              else {
                Some(makeObject(level, o))
              }
            }
            else None
          case c: ClassSymbol if !refinementClass(c) && !c.isModule => makeClass(level, c)
          case m: MethodSymbol => Some(makeMethod(level, m))
          case a: AliasSymbol => Some(makeAlias(level, a))
          case t: TypeSymbol if !t.isParam && !t.name.matches("_\\$\\d+")=> Some(makeTypeSymbol(level, t))
          case s => None
        }
      }
      else None
    }

    def makePackageObject(level: Int, symbol: ObjectSymbol): MySymbol = {
    
    
    
    
    }
    def makeObject(level: Int, symbol: ObjectSymbol): MySymbol = {
    
    println(" level :::"+ level + " object symbol" + symbol )
    
    }
    def makeMethod(level: Int, symbol: MethodSymbol): MySymbol = {
    
    println(" level :::"+ level + " object symbol" + symbol )
    
    }
    def makeAlias(level: Int, symbol: AliasSymbol): MySymbol = {
    println(" level :::"+ level + " object symbol" + symbol )
    
    
    }
    def makeTypeSymbol(level: Int, symbol: TypeSymbol): MySymbol = {
    
    println(" level :::"+ level + " object symbol" + symbol )
    
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

    private def makeChildren(level: Int, symbol: Symbol): List[MySymbol] = {
      (symbol.children flatMap (child => makeSymbol(level + 1, child))).toList
    }

    def makeModifiers(symbol: Symbol): List[Modifier] = {
      val result = ListBuffer[Modifier]()
      // print private access modifier
      if (symbol.isPrivate) result += Private
      else if (symbol.isProtected) result += Protected
      else symbol match {
        case sym: SymbolInfoSymbol => sym.symbolInfo.privateWithin match {
          case Some(t: Symbol) => result += ScopedPrivate(t.name)
          case _ =>
        }
        case _ =>
      }

      if (symbol.isSealed) result += Sealed
      if (symbol.isImplicit) result += Implicit
      if (symbol.isFinal && !symbol.isInstanceOf[ObjectSymbol]) result += Final
      if (symbol.isOverride) result += Override
      if (symbol.isAbstract) symbol match {
        case c@(_: ClassSymbol | _: ObjectSymbol) if !c.isTrait => result += Abstract
        case _ => ()
      }
      if (symbol.isCase && !symbol.isMethod) result += Case

      result.toList
    }

    private def refinementClass(c: ClassSymbol) = c.name == "<refinement>"

    def makeClass(level: Int, c: ClassSymbol): Option[MyClassDef] = {
      if (c.name == "<local child>" /*scala.tools.nsc.symtab.StdNames.LOCALCHILD.toString()*/ ) {
        None
      }
      else {
        val mods = makeModifiers(c) ::: (if (c.isTrait) List(Trait) else Nil)
        val defaultConstructor = if (c.isCase) getPrinterByConstructor(c) else ""
        val name = processName(c.name)
        val it = c.infoType
        val classType = it match {
          case PolyType(typeRef, symbols) => PolyTypeWithCons(typeRef, symbols, defaultConstructor)
          case ClassInfoType(a, b) if c.isCase => ClassInfoTypeWithCons(a, b, defaultConstructor)
          case _ => it
        }
        val selfType = c.selfType match {
          case Some(t: Type) => t
          case None => null
        }
        val children: List[MySymbol] = makeChildren(level, c)


        Some(MyClassDef(mods, name, selfType, children))
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
      val printer = new ScalaSigPrinter(stream, true)
      printer.printMethodType(m.infoType, false)(())
      baos.toString
      case None =>
      ""
      }
    }

    def printModifiers(o: Symbol) = ()
    def printChildren(level: Int, o: Symbol) = ()
    def printWithIndent(level: Int, o: String) = ()

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

        val paramEntries = mt.paramSymbols.map {
          case ms: MethodSymbol => ms.name + " : " + toString(ms.infoType)(TypeFlags(true))
          case _ => "^___^"
        }

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
      }
      else {
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

  def unpickleFromAnnotation(classFile: ClassFile, isPackageObject: Boolean): List[MyClassDef] = {
    val SCALA_SIG_ANNOTATION = "Lscala/reflect/ScalaSignature;"
    val BYTES_VALUE = "bytes"
    import classFile._
    import scalax.rules.scalasig.ClassFileParser.{ConstValueIndex, Annotation}
    import scala.reflect.generic.ByteCodecs
    classFile.annotation(SCALA_SIG_ANNOTATION) match {
      case None => Nil
      case Some(Annotation(_, elements)) =>
        val bytesElem = elements.find(elem => constant(elem.elementNameIndex) == BYTES_VALUE).get
        val bytes = ((bytesElem.elementValue match {case ConstValueIndex(index) => constantWrapped(index)}).asInstanceOf[StringBytesPair].bytes)
        val length = ByteCodecs.decode(bytes)
        val scalaSig = ScalaSigAttributeParsers.parse(ByteCode(bytes.take(length)))
        parseScalaSignature(scalaSig, isPackageObject)
    }
  }

  case class Sig(name: String, typ: MTyp)
  object Sig {
    def apply(name: String, typeFormals: List[Param], formals: List[ScalaType], returnType: ScalaType): Sig = Sig(name, MTyp(typeFormals, formals, returnType))
  }

  sealed class ScalaType
  case class MTyp(typeFormals: List[Param], formals: List[ScalaType], returnType: ScalaType) extends ScalaType
  case class Param(name: String)
  case class NamedTyp(name: String) extends ScalaType
  case class InstTyp(base: ScalaType, args: List[ScalaType]) extends ScalaType

  private val HACK = true

  def getScalaSignature(cname: String):Option[ClassDef] = {

    val cl = java.lang.Class.forName(cname).getClassLoader
    val is = (if (cl == null) java.lang.ClassLoader.getSystemClassLoader else cl).getResourceAsStream(cname.replace('.', '/') + ".class")
    val bis = new java.io.ByteArrayOutputStream
    while (is.available > 0)
    bis.write(is.read)
    val bytes = bis.toByteArray
    val reader = new ByteArrayReader(bytes)
    val cf = new Classfile(reader)

    val classname=cname

    val encName = Names.encode(if (classname == "scala.AnyRef") "java.lang.Object" else classname)

    val isPackageObject = Scalap.isPackageObjectFile(encName)

    val classFile = ClassFileParser.parse(ByteCode(bytes))

    val SCALA_SIG = "ScalaSig"

    //println("printing scalasig")

    val sig = classFile.attribute(SCALA_SIG).map(_.byteCode).map(ScalaSigAttributeParsers.parse) match {
      // No entries in ScalaSig attribute implies that the signature is stored in the annotation
      case Some(ScalaSig(_, _, entries)) if entries.length == 0 => unpickleFromAnnotation(classFile, isPackageObject)
      case Some(scalaSig) => parseScalaSignature(scalaSig, isPackageObject)
      case None => Nil
    }
    None
    }

  
    abstract sealed class Modifier
    case object Private extends Modifier
    case object Protected extends Modifier
    case class ScopedPrivate(name: String) extends Modifier
    case object Sealed extends Modifier
    case object Implicit extends Modifier
    case object Override extends Modifier
    case object Final extends Modifier
    case object Abstract extends Modifier
    case object Case extends Modifier
    case object Trait extends Modifier

  sealed abstract class MySymbol
  case class MyClassDef(modifiers: List[Modifier], name: String, selfType: Type, children: List[MySymbol]) extends MySymbol

  class ClassDef {
    var name:String=null
    var methods:List[MethodDef]=null
    var superclass:List[Class[_]]=null
    var traits:List[Class[_]]=null
    var access:String=null
    var classtype:String=null
  }

  class MethodDef {
    var name:String=null
    var returnType:FieldDef=null
    var params:List[FieldDef]=null
  }

  class FieldDef {
    var name:String=null
    var fieldType:Class[_]=null
    var fieldTypeAsString:String=null
    var fieldScalaType:ScalaType=null
  }

 }
