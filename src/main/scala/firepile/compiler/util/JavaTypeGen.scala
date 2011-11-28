package firepile.compiler.util

import java.util.StringTokenizer
import java.io._

import soot.SootClass
import soot.Body
import soot.{Unit => SootUnit}
import soot.Scene
import soot.Value
import soot.ValueBox
import soot.Local
import soot.SootClass
import soot.SootMethod
import soot.SootField
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
import soot.{Type => SootType}
import soot.jimple.toolkits.callgraph.CallGraphBuilder
import soot.jimple.toolkits.invoke.StaticInliner
import soot.jimple.toolkits.invoke.StaticMethodBinder
import soot.options.Options

import java.util.ArrayList
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import firepile.compiler.util.ClassDefs._

import scala.AnyRef
import scala.Seq
import scala.collection.mutable.HashMap

object JavaTypeGen {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("usage: JavaTypeGen classname")
      sys.exit(1)
    }
  }

  def getJavaSignature(classname: String, sootclass: SootClass): List[JavaClassDef] = {


    List(new JavaClassDef(
      classname,
      sootclass.getFields.map(i => i match {
        case j: SootField => new JavaVarDef(i.getName, i.getType)
        case _ => null
      }).toList.filter(_.isInstanceOf[JavaVarDef]),
      sootclass.getMethods.map(i => i match {
        case s: SootMethod => {
          new JavaMethodDef(i.getName, i.getReturnType,
            i.getParameterTypes.map(j => j match {
              case st: SootType => new JavaVarDef("", st)
              case _ => null
            }).toList.filter(_.isInstanceOf[JavaVarDef])
          )
        }
        case _ => null
      }).toList.filter(_.isInstanceOf[JavaMethodDef]),
      0L,
      sootclass.getInterfaces.map(i => i match {
        case s: SootClass => s
        case _ => null
      }).toList.filter(_.isInstanceOf[SootClass]),
      sootclass.getSuperclass)

    )
  }

}

