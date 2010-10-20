package firepile.compiler.util

import java.util.ArrayList
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import soot.SootClass
import soot.{Type=>SootType}
import soot.util.Chain

import scala.tools.scalap._
import scala.tools.scalap.{ Main => Scalap }
import scala.tools.scalap.scalax.rules.scalasig._

object ClassDefs {



sealed abstract class ClassDef

class JavaClassDef(
    val name: String,
    val fields: List[JavaVarDef],
    val methods: List[JavaMethodDef],
    val flags: Long,
    val interfaces: Chain[SootClass],
    val superclass: SootClass) extends ClassDef

  class JavaMethodDef(
    val name: String,
    val returnType: SootType,
    val params: List[JavaVarDef])

  class JavaVarDef(
    val name: String,
    val fieldTyp: SootType
 )
    
    class ScalaClassDef(
        val name: String,
        val classtype: String,
        val fields: List[ScalaVarDef],
        val methods: List[ScalaMethodDef],
        val superclass: List[ScalaType],
        val flags: Long,
        val innerClasses: List[ScalaClassDef],
        val scalatype: ScalaType) extends ClassDef
    
      class ScalaMethodDef(
        val name: String,
        val returnType: Type,
        val returnScalaType: ScalaType,
        val returnTypeAsString: String,
        val params: List[ScalaVarDef])
    
      class ScalaVarDef(
        val name: String,
        val fieldTyp: Type,
        val fieldType: Class[_],
        val fieldTypeAsString: String,
    	val fieldScalaType: ScalaType)
    
  sealed abstract class ScalaJavaDef
  case class ScalaJavaMethodDef(name: String, num: Int, scalaParent: String, apply: Boolean, applyInt: Int) extends ScalaJavaDef
  case class ScalaJavaClassDef(name: String, num: Int, scalaParent: String, classtype: String) extends ScalaJavaDef
  
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
    case class MyClassDef(modifiers: List[Modifier], name: String, classtype: String, selfType: Type, selfScalaType: ScalaType, fields: List[ScalaVarDef], children: List[MySymbol]) extends MySymbol
    case class MyMethodDef(name: String, selfType: Type, stringSelfType: String, children: List[MySymbol]) extends MySymbol
    case class MyVarDef(name: String, selfType: Type, stringType: String) extends MySymbol
    
      object TYPEVARIANT extends Enumeration {
          type TYPEVARIANT = Value
          val COVARIANT, CONTRAVARIANT, INVARIANT = Value
        }
    
    import TYPEVARIANT._
    
      sealed class ScalaType
      case class MTyp(typeFormals: List[Param], formals: List[ScalaType], returnType: ScalaType) extends ScalaType {
        override def equals(other: Any) = other match {
          case MTyp(tF: List[Param], f: List[ScalaType], rT: ScalaType) => if (tF == typeFormals && formals == f && returnType.equals(rT)) true else false
          case _ => false
        }
      }
      case class Param(name: String) {
        override def equals(other: Any) = other match {
          case Param(n: String) => if (n.equals(name)) true else false
          case _ => false
        }
      }
      case class NamedTyp(name: String) extends ScalaType {
        override def equals(other: Any) = other match {
          case NamedTyp(n: String) => if (n.equals(name)) true else false
          case _ => false
        }
      }
      case class ParamTyp(name: String, typ: TYPEVARIANT) extends ScalaType {
        override def equals(other: Any) = other match {
          case ParamTyp(n: String, t: TYPEVARIANT) => if (n.equals(name) && t == typ) true else false
          case _ => false
        }
      }
      case class InstTyp(base: ScalaType, args: List[ScalaType]) extends ScalaType {
        override def equals(other: Any) = other match {
          case InstTyp(b: ScalaType, a: List[ScalaType]) => {
            if ((b match {
              case NamedTyp(name: String) => if (b.asInstanceOf[NamedTyp].equals(base)) true else false
              case ParamTyp(name: String, typ: TYPEVARIANT) => if (b.asInstanceOf[ParamTyp].equals(base)) true else false
              case _ => false
            }) && a == args) true else false
          }
          case _ => false
        }
      }
  case object UnimplementedTyp extends ScalaType
  

    }