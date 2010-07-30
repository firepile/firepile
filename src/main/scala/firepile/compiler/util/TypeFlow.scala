package firepile.compiler.util


import soot.{Unit => SootUnit, Type => SootType}
import soot.toolkits.graph.UnitGraph
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.util._
import soot.jimple._
import soot.toolkits.scalar._
import soot.Body
import soot.Scene
import soot.Local
import soot.Value
import soot.ValueBox
import soot.SootClass
import soot.ArrayType
import soot.SootMethodRef
import soot.SootFieldRef
import soot.grimp.Grimp
import soot.grimp.GrimpBody
import soot.options.Options
import soot.tagkit.Tag
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import firepile.compiler.GrimpUnapply._
import firepile.compiler.util.ScalaTypeGen._

object TypeFlow {
  def main(args: Array[String]) = {
    if (args.length != 2) {
      println("usage: TypeFlow className methodSig")
      exit(1)
    }

    val className = args(0)
    val methodName = args(1)
    var b: Body = null

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

    Options.v.set_keep_line_number(true)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    val c = Scene.v.loadClassAndSupport(className)
    Scene.v.loadNecessaryClasses
    c.setApplicationClass

    for (m <- c.methodIterator)
      if (m.getName.equals(methodName))
        b = m.retrieveActiveBody
    
    val gb = Grimp.v().newBody(b, "gb")
    println("GRIMP\n" + gb)
    val g = new ExceptionalUnitGraph(gb)

    val classDefs = getScalaSignature(className.replaceAll("\\$",""))
    val tfa = new TypeFlowAnalysis(g, classDefs.head)

    for (u <- gb.getUnits) {
      decorateWithTags(u, tfa)
      for (tgs <- u.getTags) {
        tgs match {
          case t: TypeFlowTag => println("Tag: " + t.getLocalName + ":" + t.getScalaType)
          case _ => 
        }
      }
    }
  }

  class TypeFlowAnalysis(graph: UnitGraph, cls: ClassDef) extends ForwardFlowAnalysis[SootUnit,Map[String,ScalaType]](graph) {
    val emptySet: Map[String,ScalaType] = new HashMap[String,ScalaType]()

    doAnalysis

    protected def newInitialFlow(): Map[String,ScalaType] = {
        emptySet.clone
    }

    protected def entryInitialFlow(): Map[String,ScalaType] = {
      val paramMap = new HashMap[String,ScalaType]()
      val methodName = graph.getBody.getMethod.getName
      val methodDef = getMethodDefByName(methodName)
      val thisMethod = getMethodDefByName("this")

      // add class fields
      if (cls.fields != null)
        for (vd <- cls.fields)
           paramMap += vd.name -> vd.fieldScalaType


      // add params
      if( methodDef.params == null)
        println("params is null!!!")
      for (p <- methodDef.params) {
        println(" Adding param " + p.name + " = " + p.fieldScalaType)
        paramMap += (p.name -> p.fieldScalaType)
      }

      if(thisMethod.params != null)
        for (f <- thisMethod.params)
          paramMap += (f.name -> f.fieldScalaType)
      
      println("entryInitialFlow generated: " + paramMap)
      paramMap
    }

    protected def flowThrough(inValue: Map[String,ScalaType], unit: SootUnit, outValue: Map[String,ScalaType]): Unit = {
      // Compute gen from getDefs/kill set
      // Not taking into account kill set since these types will be added to tags
      outValue ++= inValue

      // kill
      for (box <- unit.getDefBoxes) {
        box.getValue match {
          case x: Local => outValue -= getName(x)
          case x => println("wtf " + x + ": " + x.getClass.getName)
        }
      }

      // gen
      unit match {
        case GIdentity(x: Local, y: Local) => 
          outValue += getName(x) -> inValue(getName(y))
        case GAssignStmt(x: Local, y: Local) => 
          outValue += getName(x) -> inValue(getName(y))

        // Need to special case: field access, call, maybe others
        // ex:
        /*
        case GAssignStmt(x: Local, GVirtualInvokeExpr(base, method, args)) => 
          outValue += getName(x) -> lookup return type (ScalaType) of base.method
        ...
          */

        case GAssignStmt(x: Local, GInstanceFieldRef(base, field)) => { // not really used? (getters)
          outValue += getName(x) -> getFieldType(base, field)
          println("Assigning " + base.getType.toString + "::" + field.name + " to " + getName(x))
        }
        case GAssignStmt(x: Local, GVirtualInvoke(base, method, _)) => {
          // Inefficient hack since getters aren't in classdef
//          getScalaSignature(base.getType.toString).head.fields.find(x => x.name.equals(method.name)) match {
//            case Some(y) => outValue += getName(x) -> y.fieldScalaType // must be a field
//            case None =>  outValue += getName(x) -> getMethodRetType(base, method) // must be a method
            outValue += getName(x) -> getMethodRetType(base, method) 
//          }
        }

        case GAssignStmt(x: Local, GArrayRef(base, _)) => { 
          // bytecodeTypeToScala(base.getType.toString) doesn't work here
          outValue += getName(x) -> bytecodeTypeToScala(base.getType.asInstanceOf[ArrayType].getArrayElementType.toString)
        }

        case GAssignStmt(x: Local, GInterfaceInvoke(base, method, _)) => {
          println("Assignment from " + base.asInstanceOf[Object].toString)
          outValue += getName(x) -> getMethodRetType(base, method)
        }

        case GAssignStmt(x: Local, GStaticInvoke(method, _)) =>
          outValue += getName(x) -> bytecodeTypeToScala(method.returnType.toString)

        case GAssignStmt(x: Local, GStaticFieldRef(field)) =>
          outValue += getName(x) -> bytecodeTypeToScala(field.`type`.toString)

        // Fall through cases: use the Java type provided by Soot.
        case GIdentity(x: Local, y) => 
          outValue += getName(x) -> bytecodeTypeToScala(y.getType.toString)
        case GAssignStmt(x: Local, y) => 
          outValue += getName(x) -> bytecodeTypeToScala(y.getType.toString)
        case x => println("wtf " + x + ": " + x.getClass.getName)
      }

/*
      */
      println("flowThrough: " + outValue)
    }

    protected def merge(in1: Map[String,ScalaType], in2: Map[String,ScalaType], out: Map[String,ScalaType]) = {
      // Compute least upper bound between matching vairables in in1 and in2, store in out
      println("MERGE")
      println("in1: " + in1)
      println("in2: " + in2)
      for (varName <- in1.keys)
        if (in2.contains(varName) && in1(varName) != in2(varName)) {
          in1(varName) match {
            case x : NamedTyp => {
               val in1SootType = (new SootClass(x.name)).getType
               val in2SootType = (new SootClass(in2(varName).asInstanceOf[NamedTyp].name)).getType
               out(varName) = NamedTyp(in1SootType.merge(in2SootType, Scene.v).toString)
            }
            case x : InstTyp => {
              x.base match {
                case y : NamedTyp => {
                  val in1SootType = (new SootClass(y.name)).getType
                  val in2SootType = (new SootClass(in2(varName).asInstanceOf[InstTyp].base.asInstanceOf[NamedTyp].name)).getType
                  out(varName) = NamedTyp(in1SootType.merge(in2SootType, Scene.v).toString)
                }
                case _ => println("InstTyp not matched")
              }
            }
            case _ => println("ScalaType not matched in merge")
          }
        }
        else
          out += (varName -> in1(varName))

      for (varName <- in2.keys)
        if (!out.contains(varName))
          out(varName) = in2(varName)
    }

    protected def copy(source: Map[String,ScalaType], dest: Map[String,ScalaType]) = {
      dest ++= source
    }

    private def getMethodDefByName(searchClass: ClassDef, name: String): MethodDef = {
      var mdef: MethodDef = null
      if ( searchClass.methods != null)
        for ( m <- searchClass.methods ) {
          if ( m.name.equals(name) )
             mdef = m
        }

      mdef
    }

    private def getMethodDefByName(name: String): MethodDef = getMethodDefByName(cls, name)

    private def bytecodeTypeToScala(bctype: String): ScalaType = {
        bctype match {
          case "boolean" => NamedTyp("scala.Boolean")
          case "byte" => NamedTyp("scala.Byte")
          case "char" => NamedTyp("scala.Char")
          case "short" => NamedTyp("scala.Short")
          case "int" => NamedTyp("scala.Int")
          case "long" => NamedTyp("scala.Long")
          case "float" => NamedTyp("scala.Float")
          case "double" => NamedTyp("scala.Double")
          case x if x.endsWith("[]") => InstTyp(NamedTyp("scala.Array"), List(bytecodeTypeToScala(x.substring(0, x.length-2))))
          case _ => NamedTyp(bctype)
        }
    }

    private def getName(local: Local): String =
      if (local.getName == "this") "this" else local.getName // + local.getNumber

    private def getMethodRetType(base: Value, method: SootMethodRef): ScalaType = {
      val cdef = getScalaSignature(method.declaringClass.toString).head
      //val cdef = getScalaSignature(base.getType.toString).head
      println("getting scalasig for " + base.getType)
      if (cdef != null)
        getMethodDefByName(cdef, method.name).returnScalaType
      else 
        bytecodeTypeToScala(method.returnType.toString)
    }

    private def getFieldType(base: Value, field: SootFieldRef): ScalaType = {
      val cdef = getScalaSignature(base.getType.toString).head
      var ftype: ScalaType = null
      if (cdef != null)
        for (vd <- cdef.fields)
          if (vd.name.equals(field.name))
             ftype = vd.fieldScalaType
      else
        ftype = bytecodeTypeToScala(field.`type`.toString)

      ftype
    }
  }

  def decorateWithTags(us: SootUnit, typeFlow: TypeFlowAnalysis) = {
    val out = typeFlow.getFlowAfter(us)
    for (u <- us.getDefBoxes)
      u.getValue match {
        case l: Local => us.addTag(new TypeFlowTag(l.getName, out(l.getName)))
      }
  }

  class TypeFlowTag(name: String, stype: ScalaType) extends Tag {
    def getName = "TypeFlowTag"
    def getValue: Array[Byte] = throw new RuntimeException("TypeFlowTag has no value for bytecode")
    def getLocalName = name
    def getScalaType = stype
  }

}

