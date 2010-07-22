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
import soot.ValueBox
import soot.SootClass
import soot.grimp.Grimp
import soot.grimp.GrimpBody
import soot.options.Options
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


    val tfa = new TypeFlowAnalysis(g, getScalaSignature(className.replaceAll("\\$","")))
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
      
      // add params
      if( methodDef.params == null)
        println("params is null!!!")
      for (p <- methodDef.params) {
        println(" Adding param " + p.name + " = " + p.fieldScalaType)
        paramMap += (p.name -> p.fieldScalaType)
      }

      // add class fields  ("this" method)
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

      for (box <- unit.getDefBoxes) {
        box.getValue match {
          case x: Local if x.getName == "this" => outValue += "this" -> bytecodeTypeToScala(x.getType.toString)
          case x: Local => {
            if(! inValue.contains(getName(x)))
              outValue += getName(x) -> bytecodeTypeToScala(x.getType.toString)
          }
          case x => println("wtf " + x + ": " + x.getClass.getName)
        }
      }
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

    private def getMethodDefByName(name: String): MethodDef = {
      var mdef: MethodDef = null

      for ( m <- cls.methods ) {
        if ( m.name.equals(name) )
           mdef = m
       }
      mdef
    }

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

    private def getName(local: Local): String = local.getName + local.getNumber

  }
}

