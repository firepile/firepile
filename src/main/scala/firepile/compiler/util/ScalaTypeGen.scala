package firepile.compiler.util

import java.util.StringTokenizer
import java.io._

import scala.tools.scalap._
import scala.tools.scalap.{ Main => Scalap }
import scala.tools.scalap.scalax.rules.scalasig._

import java.util.ArrayList
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import scala.AnyRef
import scala.Seq
import scala.collection.mutable.HashMap

object ScalaTypeGen {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("usage: ScalaTypeGen classname")
      exit(1)
    }
    val cdlist = getScalaSignature(args(0))

    if (cdlist == null) {
      println("Class Def List is null")
      exit(1)
    }
    
    printClassDef(cdlist)
    
   }
  //CF file
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
      case 'B' => (NamedTyp("scala.Byte"), i + 1)
      case 'S' => (NamedTyp("scala.Short"), i + 1)
      case 'C' => (NamedTyp("scala.Char"), i + 1)
      case 'Z' => (NamedTyp("scala.Boolean"), i + 1)
      case 'I' => (NamedTyp("scala.Int"), i + 1)
      case 'J' => (NamedTyp("scala.Long"), i + 1)
      case 'F' => (NamedTyp("scala.Float"), i + 1)
      case 'D' => (NamedTyp("scala.Double"), i + 1)
      case 'V' => (NamedTyp("scala.Unit"), i + 1)
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
        val (tpe, j) = sigToType(str, i + 1)
        (Nil, tpe, j)
      } else {
        val (tpe, j) = sigToType(str, i)
        val (rest, ret, k) = sigToType0(str, j)
        (tpe :: rest, ret, k)
      }

    def getSig(flags: Int, name: Int, tpe: Int, attribs: List[cf.Attribute]): Sig = {
      attribs find {
        case cf.Attribute(name, _) => getName(name) == "JacoMeta"
      } match {
        case Some(cf.Attribute(_, data)) =>
          val mp = new MetaParser(getName(((data(0) & 0xff) << 8) + (data(1) & 0xff)).trim())
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
  
  object TYPEVARIANT extends Enumeration {
          type TYPEVARIANT = Value
          val COVARIANT, CONTRAVARIANT, INVARIANT = Value
        }
  import TYPEVARIANT._
  
  def getVar(s:String, t: Symbol) = if (t.isCovariant) ParamTyp(s,COVARIANT) else if (t.isContravariant) ParamTyp(s,CONTRAVARIANT) else NamedTyp(s)

  def scalaType(typ: Type): ScalaType = {
    typ match {
      case NoType => NamedTyp("NoType")
      case NoPrefixType => NamedTyp("java.lang.object")
      case ThisType(symbol: Symbol) => if (symbol.path.indexOf("<empty>") >= 0) getVar(symbol.name,symbol) else getVar(symbol.path,symbol)
      case SingleType(typeRef: Type, singleTypeSymbol: Symbol) => if (singleTypeSymbol.path.indexOf("<empty>") >= 0) getVar(singleTypeSymbol.name,singleTypeSymbol) else getVar(singleTypeSymbol.path,singleTypeSymbol)
      case ConstantType(constant: Any) => NamedTyp("scala.Any")
      case TypeRefType(prefix: Type, symbol: Symbol, typeArgs: Seq[Type]) => if (typeArgs.isEmpty) { if (symbol.path.indexOf("<empty>") >= 0) getVar(symbol.name,symbol) else getVar(symbol.path,symbol) } else InstTyp((if (symbol.path.indexOf("<empty>") >= 0) getVar(symbol.name,symbol) else getVar(symbol.path,symbol)), typeArgs.map(i => scalaType(i)).toList)
      case TypeBoundsType(lower: Type, upper: Type) => NamedTyp("TyeBoundsType")
      case RefinedType(classSym: Symbol, typeRefs: List[Type]) => NamedTyp("RefinedType")
      case ClassInfoType(symbol: Symbol, typeRefs: Seq[Type]) => NamedTyp(" ClassInfoType")
      case ClassInfoTypeWithCons(symbol: Symbol, typeRefs: Seq[Type], cons: String) => NamedTyp("ClassInfoType")
      case MethodType(resultType: Type, paramSymbols: Seq[Symbol]) => NamedTyp("MethodType")
      case PolyType(typeRef: Type, symbols: Seq[TypeSymbol]) => scalaType(typeRef)
      case PolyTypeWithCons(typeRef: Type, symbols: Seq[TypeSymbol], cons: String) => NamedTyp("PolyType")
      case ImplicitMethodType(resultType: Type, paramSymbols: Seq[Symbol]) => NamedTyp("ImplicitType")
      case AnnotatedType(typeRef: Type, attribTreeRefs: List[Int]) => scalaType(typeRef)
      case AnnotatedWithSelfType(typeRef: Type, symbol: Symbol, attribTreeRefs: List[Int]) => NamedTyp("AnnotatedType")
      case DeBruijnIndexType(typeLevel: Int, typeIndex: Int) => NamedTyp(" DebruijnIndex")
      case ExistentialType(typeRef: Type, symbols: Seq[Symbol]) => NamedTyp(" ExistentialType")
    }
  }

  def getScalaSignature(cname: String): List[ClassDef] = {

    val cl = java.lang.Class.forName(cname).getClassLoader
    val is = (if (cl == null) java.lang.ClassLoader.getSystemClassLoader else cl).getResourceAsStream(cname.replace('.', '/') + ".class")
    val bis = new java.io.ByteArrayOutputStream
    while (is.available > 0)
      bis.write(is.read)
    val bytes = bis.toByteArray
    val reader = new ByteArrayReader(bytes)
    val cf = new Classfile(reader)

    val classname = cname

    val encName = Names.encode(if (classname == "scala.AnyRef") "java.lang.Object" else classname)
    val isPackageObject = Scalap.isPackageObjectFile(encName)
    val classFile = ClassFileParser.parse(ByteCode(bytes))
    val SCALA_SIG = "ScalaSig"

    val sig = classFile.attribute(SCALA_SIG).map(_.byteCode).map(ScalaSigAttributeParsers.parse) match {
      // No entries in ScalaSig attribute implies that the signature is stored in the annotation
      case Some(ScalaSig(_, _, entries)) if entries.length == 0 => unpickleFromAnnotation(classFile, isPackageObject)
      case Some(scalaSig) => parseScalaSignature(scalaSig, isPackageObject)
      case None => None
    }
    
 
     sig match {

      case List(myclassdef: MyClassDef) => List(getClassDef(myclassdef))
      case scala.None => null
      case _ => getListClassDef(sig.asInstanceOf[List[MyClassDef]])

    }

  }

  def getListClassDef(myClassDefList: List[MyClassDef]): List[ClassDef] = {
  
    var fieldList = ListBuffer[ClassDef]()

    for (i <- myClassDefList)
      fieldList += getClassDef(i)

    fieldList.toList

  }

  def getClassDef(myClassDef: MyClassDef): ClassDef = {
  
  var innerClasses = ListBuffer[ClassDef]()
  
    myClassDef match {
      case MyClassDef(modifiers: List[Modifier], name: String, classtype: String, selfType: Type, fields: List[VarDef], children: List[MySymbol]) => {
        new ClassDef(
          name,
          classtype,
          fields, 
          (children.map { child => child match { 
            case MyMethodDef(name, returnType, stringReturnType, params) => {
                new MethodDef((if (name == "<init>" || name == "$init$") "this" else name),
                returnType,
                scalaType(returnType),
                stringReturnType, (params.map {
                  case MyVarDef(name, varType, stringVarType) => {
                    new VarDef(name, varType, null, stringVarType, scalaType(varType))
                  }
                  case _ => null
                }.toList).filter(_.isInstanceOf[VarDef]))
             }
               case MyClassDef(innerModifiers: List[Modifier], innerName: String, innerClasstype: String, innerSelfType: Type, innerFields: List[VarDef], innerChildren: List[MySymbol]) => innerClasses+=getClassDef(child.asInstanceOf[MyClassDef]); null
          }}.toList).filter(_.isInstanceOf[MethodDef]), 
          (selfType match {
            case TypeRefType(prefix: Type, symbol: Symbol, typeRefs: Seq[Type]) => typeRefs.map { i => scalaType(i) }.toList
            case ClassInfoType(symbol, typeRefs) => typeRefs.map { i => scalaType(i) }.toList
            case PolyType(ClassInfoType(symbol, typeRefs), symbols: Seq[TypeSymbol]) => typeRefs.map { i => scalaType(i) }.toList
          }),
          (selfType match {
            case TypeRefType(_,TypeSymbol(SymbolInfo(_, _, flags: Int, _, _, _)),_) => flags
            case ClassInfoType(ClassSymbol(SymbolInfo(_, _, flags: Int, _, _, _), _), _) => flags
            case PolyType(ClassInfoType(ClassSymbol(SymbolInfo(_, _, flags: Int, _, _, _), _), _), symbols: Seq[TypeSymbol]) => flags
          }),
          innerClasses.toList.filter(_.isInstanceOf[ClassDef])
          )
      }
    }
  }

 def printClassDef(cdList: List[ClassDef]) : Unit = { 
 
 println("---------------------------------------------------------------------------------------")
    for (cd <- cdList) {
 
       println("Class Name::" + cd.name)
       println("Class Type::" + cd.classtype)
 
       if (cd.fields != null)
         for (i <- cd.fields)
           println("Class Field Name:" + i.name + "Class Field As String::" + i.fieldTypeAsString + "Class Field As Scala Type::" + i.fieldScalaType)
       
       println("------Super Classes----------")
       for (i <- cd.superclass)
         println("Class Name::" + i)
         
          println("---------------Methods-------------------")
	  
	        for (i <- 0 until cd.methods.length) {
	          var m = cd.methods(i)
	          println("Method Name:::" + m.name)
	          if (m.returnType != null)
	            println("Return Type::" + m.returnType)
	          if (m.returnTypeAsString != null)
	            println("Return Type As String::" + m.returnTypeAsString)
	          if (m.returnScalaType != null)
	            println("Return Type as Scala Type::" + m.returnScalaType)
	            
	          var p = m.params
	          if (p != null) {
	            var cc = p.length
	            for (j <- 0 until cc) {
	              if (p(j) != null) {
	                if (p(j).name != null)
	                  println("Parameter name::" + p(j).name)
	                if (p(j).fieldTyp != null)
	                  println("Parameter Type::" + p(j).fieldTyp)
	                if (p(j).fieldTypeAsString != null)
	                  println("Parameter Type As String::" + p(j).fieldTypeAsString)
	                if (p(j).fieldScalaType != null)
	                  println("Parameter Type As ScalaType::" + p(j).fieldScalaType)
	              }
	            }
	          }
 
       if(!cd.innerClasses.isEmpty){
       println("------------------------Inner Classes -----------------------------------")
       printClassDef(cd.innerClasses)
       }
 
      
       }
 
     }

 
 }
 
  def unpickleFromAnnotation(classFile: ClassFile, isPackageObject: Boolean): List[MyClassDef] = {
    val SCALA_SIG_ANNOTATION = "Lscala/reflect/ScalaSignature;"
    val BYTES_VALUE = "bytes"
    import classFile._
    import scalax.rules.scalasig.ClassFileParser.{ ConstValueIndex, Annotation }
    import scala.reflect.generic.ByteCodecs
    classFile.annotation(SCALA_SIG_ANNOTATION) match {
      case None => Nil
      case Some(Annotation(_, elements)) =>
        val bytesElem = elements.find(elem => constant(elem.elementNameIndex) == BYTES_VALUE).get
        val bytes = ((bytesElem.elementValue match { case ConstValueIndex(index) => constantWrapped(index) }).asInstanceOf[StringBytesPair].bytes)
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
  case class MTyp(typeFormals: List[Param], formals: List[ScalaType], returnType: ScalaType) extends ScalaType {
  override def equals(other: Any ) = other match {
  case MTyp(tF: List[Param], f: List[ScalaType], rT: ScalaType) => if ( tF ==typeFormals && formals == f && returnType.equals(rT)) true else false
  case _ => false
   }
  }
  case class Param(name: String) {
  override def equals(other: Any) = other match {
  case Param(n: String) => if(n.equals(name)) true else false
  case _ => false
   }
  }
  case class NamedTyp(name: String) extends ScalaType {
  override def equals(other: Any) = other match {
  case NamedTyp(n: String) => if(n.equals(name)) true else false
  case _ => false
   }
  }
  case class ParamTyp(name: String, typ: TYPEVARIANT) extends ScalaType {
  override def equals(other: Any) = other match {
  case ParamTyp(n: String, t: TYPEVARIANT) => if(n.equals(name) && t == typ ) true else false
  case _ => false
     }
  }
  case class InstTyp(base: ScalaType, args: List[ScalaType]) extends ScalaType {
  override def equals(other: Any) = other match {
  case InstTyp(b: ScalaType, a: List[ScalaType]) => { if ( (b match { case NamedTyp(name: String) => if(b.asInstanceOf[NamedTyp].equals(base)) true else false 
                                                                      case ParamTyp(name: String, typ: TYPEVARIANT) => if(b.asInstanceOf[ParamTyp].equals(base)) true else false
                                                                      case _ => false }) && a == args ) true else false }
  case _ => false
     }
  }
  case object UnimplementedTyp extends ScalaType 

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

  class ClassDef(
    val name: String,
    val classtype: String,
    val fields: List[VarDef],
    val methods: List[MethodDef],
    val superclass: List[ScalaType],
    val flags: Long,
    val innerClasses: List[ClassDef])

  class MethodDef(
    val name: String,
    val returnType: Type,
    val returnScalaType: ScalaType,
    val returnTypeAsString: String,
    val params: List[VarDef])

  class VarDef(
    val name: String,
    val fieldTyp: Type,
    val fieldType: Class[_],
    val fieldTypeAsString: String,
    val fieldScalaType: ScalaType)

  sealed abstract class MySymbol
  case class MyClassDef(modifiers: List[Modifier], name: String, classtype: String, selfType: Type, fields: List[VarDef], children: List[MySymbol]) extends MySymbol
  case class MyMethodDef(name: String, selfType: Type, stringSelfType: String, children: List[MySymbol]) extends MySymbol
  case class MyVarDef(name: String, selfType: Type, stringType: String) extends MySymbol

  def parseScalaSignature(scalaSig: ScalaSig, isPackageObject: Boolean): List[MyClassDef] = {
    val syms = scalaSig.topLevelClasses ::: scalaSig.topLevelObjects
    val pkgPath = syms.head.parent match {
      //Partial match
      case Some(p) if (p.name != "<empty>") => {
        val path = p.path
        if (!isPackageObject) {
          path
        } else {
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

    import java.util.regex.Pattern

    import scala.tools.scalap.scalax.util.StringUtil
    import reflect.NameTransformer
    import java.lang.String

    val baos = new ByteArrayOutputStream
    val stream = new PrintStream(baos)

    val CONSTRUCTOR_NAME = "<init>"

    case class TypeFlags(printRep: Boolean)

    def makeClassDef(symbol: Symbol) = makeSymbol(0, symbol) match {
      case Some(x: MyClassDef) => Some(x)
      case _ => None
    }

    def makeSymbol(symbol: Symbol) { makeSymbol(0, symbol) }

    def makeSymbol(level: Int, symbol: Symbol): Option[MySymbol] = {

      if (!symbol.isLocal) {
        // TODO
        symbol match {
          case o: ObjectSymbol =>
            if (!isCaseClassObject(o)) {
              if (o.name == "package") {
                Some(makePackageObject(level, o))
              } else {
                Some(makeObject(level, o))

              }
            } else None
          case c: ClassSymbol if !refinementClass(c) && !c.isModule => makeClass(level, c)
          case m: MethodSymbol => Some(makeMethod(level, m))
          case a: AliasSymbol => Some(makeAlias(level, a))
          case t: TypeSymbol if !t.isParam && !t.name.matches("_\\$\\d+") => Some(makeTypeSymbol(level, t))
          case s => None
        }
      } else None
    }

    private def refinementClass(c: ClassSymbol) = c.name == "<refinement>"

    def makeClass(level: Int, c: ClassSymbol): Option[MyClassDef] = {

      var t: Type = NoType
      if (c.name == "<local child>" /*scala.tools.nsc.symtab.StdNames.LOCALCHILD.toString()*/ ) {
        None
      } else {
        val mods = makeModifiers(c) ::: (if (c.isTrait) List(Trait) else Nil)
        val defaultConstructor = if (c.isCase) getPrinterByConstructor(c) else ""
        val name = c match { 
                   case ClassSymbol(SymbolInfo(_, owner : Symbol,_, _, _, _),_) => if(!owner.path.equals("<empty>")) owner.path.replace("<empty>.","") +"."+ processName(c.name) else processName(c.name)
                   case _ => processName(c.name) } 
        t = c.infoType
        val classType = t match {
          case PolyType(typeRef, symbols) => PolyTypeWithCons(typeRef, symbols, defaultConstructor)
          case ClassInfoType(a, b) if c.isCase => ClassInfoTypeWithCons(a, b, defaultConstructor)
          case _ => null
        }
        val selfType = c.selfType match {
          case Some(typ: Type) => t = typ
          case None => null
        }

        var fieldList = ListBuffer[VarDef]()
        val newchildren: List[MySymbol] = makeChildren(level, c).map { i =>
          i match {
            case MyMethodDef(name, rType, rString, children) => if (rString.equals("field_$eq")) {
              fieldList += new VarDef(name, rType, null, toString(rType)(TypeFlags(true)), scalaType(rType))
              MyMethodDef(name, rType, toString(rType)(TypeFlags(true)), children)
            } else i
            case MyClassDef(modifiers: List[Modifier], name: String, classtype: String, selfType: Type, fields: List[VarDef], children: List[MySymbol]) => i
            case _ => null
          }
        }.toList

        Some(MyClassDef(mods, name, if (c.isCase && !c.isMethod) "case" else if (c.isTrait) "trait" else "class", t, fieldList.toList, newchildren.filter(_.isInstanceOf[MySymbol])))
      }
    }

    private def makeChildren(level: Int, symbol: Symbol): List[MySymbol] = {
      (symbol.children flatMap (child => makeSymbol(level + 1, child))).toList
    }

    def makeModifiers(symbol: Symbol): List[Modifier] = {

      val result = ListBuffer[Modifier]()
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

    def makePackageObject(level: Int, o: ObjectSymbol): MySymbol = {
      
      val mod = makeModifiers(o)
      val name = o.symbolInfo.owner.name
      val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
      val t = makeType(classSymbol)
      val children = makeChildren(level, classSymbol)
      MyClassDef(mod, name, "object", t, null, children)

    }

    def makeObject(level: Int, o: ObjectSymbol): MySymbol = {

      val mod = makeModifiers(o)
      val name = o match { 
                   case ObjectSymbol(SymbolInfo(_, owner : Symbol,_, _, _, _)) =>if(!owner.path.equals("<empty>")) owner.path.replace("<empty>.","") +"."+ o.name else o.name
                   case _ => o.name } 
                   
      val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
      val t = makeType(classSymbol)
      var fieldList = ListBuffer[VarDef]()
      val newchildren: List[MySymbol] = makeChildren(level, classSymbol).map { i =>
        i match {
          case MyMethodDef(name, rType, rString, children) => if (rString.equals("field_$eq")) {
            fieldList += new VarDef(name, rType, null, toString(rType)(TypeFlags(true)), scalaType(rType))
            MyMethodDef(name, rType, toString(rType)(TypeFlags(true)), children)
          } else i
          case _ => i
        }
      }.toList
      MyClassDef(mod, name, "object", t, fieldList.toList, newchildren.filter(_.isInstanceOf[MySymbol]))
    }

    def makeMethodType(t: Type, printResult: Boolean): (Type, String, List[MyVarDef]) = {

      def _pmt(mt: Type { def resultType: Type; def paramSymbols: Seq[Symbol] }): (Type, String, List[MyVarDef]) = {

        val paramEntries = mt.paramSymbols.map({
          case ms: MethodSymbol => MyVarDef(ms.name, ms.infoType, toString(ms.infoType)(TypeFlags(true)))
        }).toList

        val (returnType: Type, returnTypeString: String, param: List[MyVarDef]) = mt.resultType match {
          case mt: MethodType => makeMethodType(mt, printResult)
          case imt: ImplicitMethodType => makeMethodType(imt, printResult)
          case x => (mt.resultType, toString(mt.resultType)(TypeFlags(true)), paramEntries)
        }

        (returnType, returnTypeString, paramEntries)
      }

      t match {
        case mt@MethodType(resType, paramSymbols) => _pmt(mt)
        case mt@ImplicitMethodType(resType, paramSymbols) => _pmt(mt)
        case pt@PolyType(mt, typeParams) => makeMethodType(mt, printResult)
        //todo consider another method types
        case x => (makeType(x), "NoType", List(null))
      }

    }

    def makeMethod(level: Int, m: MethodSymbol): MySymbol = {

      if (underCaseClass(m) && m.name == CONSTRUCTOR_NAME) null
      if (m.name.matches(".+\\$default\\$\\d+")) null // skip default function parameters
      if (m.name.startsWith("super$")) null // do not print auxiliary qualified super accessors

      if (m.isAccessor) {
        val indexOfSetter = m.parent.get.children.indexWhere(x => x.isInstanceOf[MethodSymbol] &&
          x.asInstanceOf[MethodSymbol].name == m.name + "_$eq")
      }
      val (rType, rString, c) = m.name match {
        case CONSTRUCTOR_NAME => {
          makeMethodType(m.infoType, false)
        }
        case name => {
          makeMethodType(m.infoType, false)
        }
      }
      MyMethodDef(m.name, rType, if (m.isAccessor && !(m.name.indexOf("_$eq") > 0)) "field_$eq" else rString, c)

    }

    def makeAlias(level: Int, a: AliasSymbol): MySymbol = {
      val name = a.name
      val t = makeType(a.infoType)
      val children = makeChildren(level, a)
      MyMethodDef(name, t, toString(t)(TypeFlags(true)), children)
    }

    def makeTypeSymbol(level: Int, t: TypeSymbol): MySymbol = {

      val typ = makeType(t.infoType)
      MyMethodDef("type", typ, toString(typ)(TypeFlags(true)), null)
    }

    def isCaseClassObject(o: ObjectSymbol): Boolean = {
      val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
      o.isFinal && (classSymbol.children.find(x => x.isCase && x.isInstanceOf[MethodSymbol]) match {
        case Some(_) => true
        case None => false
        case _ => { println(" control can come here as well"); false }
      })
    }

    private def underCaseClass(m: MethodSymbol) = m.parent match {
      case Some(c: ClassSymbol) => c.isCase
      case _ => false
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

    implicit object _tf extends TypeFlags(false)

    def toString(attrib: AttributeInfo): String = {
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

    def makeType(sym: SymbolInfoSymbol)(implicit flags: TypeFlags): Type = makeType(sym.infoType)(flags)

    def makeType(t: Type)(implicit flags: TypeFlags): Type = t

    def makeType(t: Type, sep: String)(implicit flags: TypeFlags): Type = t

    def toString(sym: SymbolInfoSymbol)(implicit flags: TypeFlags): String = toString(sym.infoType)(flags)

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
        case RefinedType(classSym, typeRefs) => sep + typeRefs.map(toString)
        case ClassInfoType(symbol, typeRefs) => sep + typeRefs.map(toString)
        case ClassInfoTypeWithCons(symbol, typeRefs, cons) => sep + typeRefs.map(toString)
        case ImplicitMethodType(resultType, _) => toString(makeType(resultType, sep))
        case MethodType(resultType, _) => toString(makeType(resultType, sep))

        case PolyType(typeRef, symbols) => typeParamString(symbols) + makeType(typeRef, sep)
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

}

