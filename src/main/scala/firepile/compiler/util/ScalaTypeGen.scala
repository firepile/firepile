
package firepile.compiler.util

import java.util.StringTokenizer
import java.io._

import scala.tools.scalap._
import scala.tools.scalap.{Main => Scalap}
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
    val cd=getScalaSignature(args(0))
    
    if(cd!=null)
    println(" class def ::"+cd)
     else {
     println(" class def is null")
     exit(1)
     }
     
   if(cd.name!=null)
    println(" ::class name ::"+ cd.name)
    if(cd.flags!=null)
     println(" :: class flag ::"+cd.flags)
    
    
    if(cd.fields!=null)
    for(i <- cd.fields)
    println(" class field name ::"+i.name+" class field string ::"+i.fieldTypeAsString+"  class field scala type::"+ i.fieldScalaType)
    
    for(i <- cd.superclass)
    println(" super class name::"+i)
    
    println(" ::Methods ::")

    for (i<-0 until cd.methods.length){
      var m=cd.methods(i)
        println(" Method name:::"+m.name)
           if (m.returnType!=null)
        println(" Return Type :"+m.returnType)
        if (m.returnTypeAsString!=null)
        println(" Return Type As String::"+m.returnTypeAsString)
        if(m.returnScalaType!=null)
        println(" Return Type as Scala Type::"+m.returnScalaType)
      var p=m.params
      if (p!=null){
        var cc=p.length
        for(j <-0 until cc){
          if(p(j)!=null){
            if(p(j).name!=null)
              println(" Param name::"+p(j).name)
            if(p(j).fieldTyp!=null)
              println(" Param Type name::"+p(j).fieldTyp)
               if(p(j).fieldTypeAsString!=null)
              println(" Param Type name as String::"+p(j).fieldTypeAsString)
            if(p(j).fieldScalaType!=null)
              println(" Param Type As ScalaType::"+p(j).fieldScalaType)
          }
        }
      }

    }
    

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
  
  def scalaType(typ: Type) : ScalaType = {
  
  typ match {
    case  NoType => NamedTyp("NoType")
    case  NoPrefixType => NamedTyp("java.lang.object")
    case  ThisType(symbol : Symbol)  => if(symbol.path.indexOf("<empty>")>=0) NamedTyp(symbol.name) else NamedTyp(symbol.path)
    case  SingleType(typeRef : Type, singleTypeSymbol : Symbol) => if(singleTypeSymbol.path.indexOf("<empty>")>=0) NamedTyp(singleTypeSymbol.name) else NamedTyp(singleTypeSymbol.path)
    case  ConstantType(constant : Any)  => NamedTyp("scala.Any")
    case  TypeRefType(prefix : Type, symbol : Symbol, typeArgs : Seq[Type])  => if(typeArgs.isEmpty) { if(symbol.path.indexOf("<empty>")>=0) NamedTyp(symbol.name) else NamedTyp(symbol.path) }else InstTyp(NamedTyp(if(symbol.path.indexOf("<empty>")>=0) symbol.name else symbol.path), typeArgs.map(i=>scalaType(i)).toList) 
    case  TypeBoundsType(lower : Type, upper : Type)  => NamedTyp("tyeboundstype")
    case  RefinedType(classSym : Symbol, typeRefs : List[Type])  => NamedTyp("refined type")
    case  ClassInfoType(symbol : Symbol, typeRefs : Seq[Type])  => NamedTyp(" Class info type")
    case  ClassInfoTypeWithCons(symbol : Symbol, typeRefs : Seq[Type], cons: String)  => NamedTyp("class info type with symbols")
    case  MethodType(resultType : Type, paramSymbols : Seq[Symbol])  => NamedTyp(" method type")
    case  PolyType(typeRef : Type, symbols : Seq[TypeSymbol])  => NamedTyp("poly type")
    case  PolyTypeWithCons(typeRef : Type, symbols : Seq[TypeSymbol], cons: String)  => NamedTyp("poly type with cons")
    case  ImplicitMethodType(resultType : Type, paramSymbols : Seq[Symbol])  => NamedTyp("Implicit Type")
    case  AnnotatedType(typeRef : Type, attribTreeRefs : List[Int])  => NamedTyp("Annotated Type")
    case  AnnotatedWithSelfType(typeRef : Type, symbol : Symbol, attribTreeRefs : List[Int])  => NamedTyp("Annotated type with self type")
    case  DeBruijnIndexType(typeLevel : Int, typeIndex : Int)  => NamedTyp(" Debruijn Index")
    case  ExistentialType(typeRef : Type, symbols : Seq[Symbol])  => NamedTyp(" Existential type")
  }
}

 def getScalaSignature(cname: String): ClassDef = {

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

      val sig = classFile.attribute(SCALA_SIG).map(_.byteCode).map(ScalaSigAttributeParsers.parse) match {
        // No entries in ScalaSig attribute implies that the signature is stored in the annotation
        case Some(ScalaSig(_, _, entries)) if entries.length == 0 => unpickleFromAnnotation(classFile, isPackageObject)
        case Some(scalaSig) => parseScalaSignature(scalaSig, isPackageObject)
        case None => None
      }
 
     sig match {

        case List(myclassdef :MyClassDef) => getClassDef(myclassdef) 
        case _ => null
      }

    }

     def getClassDef(myclassdef: MyClassDef) : ClassDef = {
      
        myclassdef match {
          case MyClassDef(modifiers: List[Modifier], name: String, selfType: Type, fields: List[VarDef], children: List[MySymbol]) => {
          new ClassDef(
          name,
          fields,
         (children.map {
                case MyMethodDef(name, returnType, stringReturnType, params) => {
                new MethodDef(
                  (if (name == "<init>") "this" else name),
                  returnType,
                  scalaType(returnType),
                  stringReturnType,
                  params.map {
                      case MyVarDef(name, varType, stringVarType) => {
                        new VarDef(name,varType,null,stringVarType,scalaType(varType))
                      }
                   }.toList)
                  }
              }.toList),
              (selfType match { 
              case ClassInfoType(symbol, typeRefs) => typeRefs.map{i=> scalaType(i)}.toList}),
              null,
              (selfType match {
              case ClassInfoType(ClassSymbol(SymbolInfo(_, _, flags : Int, _ , _, _) , _ ), _) => flags })
              )
            }
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
        val name:String,
        val fields:List[VarDef],
        val methods:List[MethodDef],
        val superclass:List[ScalaType],
        val traits:List[ScalaType],
        val flags:Long
      )

      class MethodDef (
        val name:String,
        val returnType:Type,
        val returnScalaType:ScalaType,
        val returnTypeAsString:String,
        val params:List[VarDef]
      )

      class VarDef (
        val name:String,
        val fieldTyp:Type,
        val fieldType:Class[_],
        val fieldTypeAsString:String,
        val fieldScalaType:ScalaType
      )


      sealed abstract class MySymbol
      case class MyClassDef(modifiers: List[Modifier], name: String, selfType: Type, fields: List[VarDef], children: List[MySymbol]) extends MySymbol
      case class MyMethodDef(name:String, selfType: Type, stringSelfType: String, children: List[MySymbol]) extends MySymbol
      case class MyVarDef(name:String, selfType: Type, stringType: String) extends MySymbol


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

        def makeSymbol(symbol: Symbol) {makeSymbol(0, symbol)}

        def makeSymbol(level: Int, symbol: Symbol): Option[MySymbol] = {
        
          if (!symbol.isLocal) {
            // TODO
            symbol match {
              case o: ObjectSymbol =>
              if (!isCaseClassObject(o)) {
                if (o.name == "package") {
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



        private def refinementClass(c: ClassSymbol) = c.name == "<refinement>"

        def makeClass(level: Int, c: ClassSymbol): Option[MyClassDef] = {
          var t:Type = NoType
          if (c.name == "<local child>" /*scala.tools.nsc.symtab.StdNames.LOCALCHILD.toString()*/ ) {
            None
          }
          else {
            val mods = makeModifiers(c) ::: (if (c.isTrait) List(Trait) else Nil)
              val defaultConstructor = if (c.isCase) getPrinterByConstructor(c) else ""
            val name = processName(c.name)
              t = c.infoType
            val classType = t match {
              case PolyType(typeRef, symbols) => PolyTypeWithCons(typeRef, symbols, defaultConstructor)
              case ClassInfoType(a, b) if c.isCase => ClassInfoTypeWithCons(a, b, defaultConstructor)
              case _ => null
            }
            val selfType = c.selfType match {
              case Some(typ: Type) => t=typ
              case None => null
            }
                
            var fieldList =ListBuffer[VarDef]()
            val newchildren:List[MySymbol] = makeChildren(level, c).map {i =>
            i match{
            case MyMethodDef(name,rType,rString,children) => if(rString.equals("field_$eq")) { if(!(name.indexOf("_$eq")>0)) {fieldList+= new VarDef(name,rType,null,toString(rType)(TypeFlags(true)),scalaType(rType)) 
                                                             null } else null } else i
               }
            }.toList

            Some(MyClassDef(mods, "class "+name, t, fieldList.toList, newchildren.filter(_.isInstanceOf[MySymbol])))
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


        def makePackageObject(level: Int, o: ObjectSymbol) : MySymbol = {
          val mod=makeModifiers(o)
          val name = o.symbolInfo.owner.name
          val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
          val t=makeType(classSymbol)
          val children=makeChildren(level, classSymbol)
          MyClassDef(mod, name, t,null, children)

        }

        def makeObject(level: Int, o: ObjectSymbol) : MySymbol = {
          val mod=makeModifiers(o)
          val name=o.name
          val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
          val t=makeType(classSymbol)
          var fieldList =ListBuffer[VarDef]()
	      val newchildren:List[MySymbol] = makeChildren(level, classSymbol).map {i =>
	      i match{
	      case MyMethodDef(name,rType,rString,children) => if(rString.equals("field_$eq")) { if(!(name.indexOf("_$eq")>0)) {fieldList+= new VarDef(name,rType,null,toString(rType)(TypeFlags(true)),scalaType(rType)) 
							       null } else null } else i
		 }
	      }.toList
          MyClassDef(mod, "object "+name, t,fieldList.toList,newchildren.filter(_.isInstanceOf[MySymbol]))
        }


       def makeMethodType(t: Type, printResult: Boolean) : (Type, String, List[MyVarDef]) = {

        def _pmt(mt: Type {def resultType: Type; def paramSymbols: Seq[Symbol]}) : (Type, String, List[MyVarDef]) = {

          val paramEntries = mt.paramSymbols.map({
              case ms: MethodSymbol => MyVarDef(ms.name,ms.infoType,toString(ms.infoType)(TypeFlags(true)))
            }).toList

          /*
          // TO DO 
          val (returntype,param)=mt.resultType match {
            case mt: MethodType => makeMethodType(mt, printResult)
            case imt: ImplicitMethodType => makeMethodType(imt, printResult)
            case x => if (printResult) {
              //print(" : ");
              (makeType(x),List(MyVarDef("",NoType)))
            }
          }
          */
           (mt.resultType,toString(mt.resultType)(TypeFlags(true)),paramEntries)
        }
       t match {
          case mt@MethodType(resType, paramSymbols) => _pmt(mt)
          case mt@ImplicitMethodType(resType, paramSymbols) =>_pmt(mt) 
          case pt@PolyType(mt, typeParams) => {
            makeMethodType(mt, printResult)
          }
          //todo consider another method types
          case x => (makeType(x),toString(x)(TypeFlags(true)),List(MyVarDef("",NoType,"notype")))
        }

        }

      def makeMethod(level: Int, m: MethodSymbol) : MySymbol = {
       if (underCaseClass(m) && m.name == CONSTRUCTOR_NAME) None.asInstanceOf[MySymbol]
        if (m.name.matches(".+\\$default\\$\\d+")) None.asInstanceOf[MySymbol] // skip default function parameters
        if (m.name.startsWith("super$")) None.asInstanceOf[MySymbol] // do not print auxiliary qualified super accessors

        if (m.isAccessor) {
          val indexOfSetter = m.parent.get.children.indexWhere(x => x.isInstanceOf[MethodSymbol] &&
            x.asInstanceOf[MethodSymbol].name == m.name + "_$eq")
         }
        val (rType,rString,c)= m.name match {
          case CONSTRUCTOR_NAME =>{
            makeMethodType(m.infoType,false)
             }
          case name => {
           makeMethodType(m.infoType,false)
            } 
        }
        MyMethodDef(m.name,rType,if(m.isAccessor) "field_$eq" else rString,c)
        
      }

      def makeAlias(level: Int, a: AliasSymbol) : MySymbol = {
        val name=a.name
        val t=makeType(a.infoType)
        val children=makeChildren(level, a)
        MyClassDef(null, name, t, null, children)
      }

      def makeTypeSymbol(level: Int, t: TypeSymbol) : MySymbol = {
        val typ= makeType(t.infoType)
        MyClassDef(null,"type",typ,null,null)
      }

      def isCaseClassObject(o: ObjectSymbol): Boolean = {
        val TypeRefType(prefix, classSymbol: ClassSymbol, typeArgs) = o.infoType
        o.isFinal && (classSymbol.children.find(x => x.isCase && x.isInstanceOf[MethodSymbol]) match {
            case Some(_) => true
            case None => false
            case _ => {println(" control can come here as well"); false}
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

      def makeType(sym: SymbolInfoSymbol)(implicit flags: TypeFlags): Type = makeType(sym.infoType)(flags)

      def makeType(t: Type)(implicit flags: TypeFlags): Type = t

      def makeType(t: Type, sep: String)(implicit flags: TypeFlags): Type  = t

      def toString(sym: SymbolInfoSymbol)(implicit flags: TypeFlags): String = toString(sym.infoType)(flags)
      
      def toString(t: Type)(implicit flags: TypeFlags): String = toString(t, "")(flags)
      
      def toString(t: Type, sep: String)(implicit flags: TypeFlags) : String = {
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



