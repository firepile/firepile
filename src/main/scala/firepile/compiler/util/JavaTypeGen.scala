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
import soot.{Type=>SootType}
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
      exit(1)
    }
//   setup
//   val cdlist = getJavaSignature(args(0),new SootClass(args(0)))
  }
  
  /*
    
  class JavaClassDef(
      val name: String,
      val fields: List[JavaVarDef],
      val methods: List[JavaMethodDef],
      val flags: Long,
      val interfaces: List[SootClass],
      val superclass: SootClass) extends ClassDef
  
    class JavaMethodDef(
      val name: String,
      val returnType: Type,
      val params: List[JavaVarDef])
  
    class JavaVarDef(
      val name: String,
      val fieldTyp: Type,
   )
    
    */

def getJavaSignature( classname: String, sootclass: SootClass) : List[JavaClassDef] = { 

 var fieldList = ListBuffer[JavaVarDef]()
 var methodList= ListBuffer[JavaMethodDef]()
 
 for(i <- sootclass.getFields)
    fieldList+= new JavaVarDef(i.getName,i.getType)
 
 for(i <- sootclass.getMethods){
  var parList = ListBuffer[JavaVarDef]()
  var a = ListBuffer[soot.Type]()
  for(k <- 0 until i.getParameterTypes.length)
  a+= i.getParameterType(k) 
  //val a: List[soot.Type]= i.getParameterTypes
  for(j <- a)
  parList += (new JavaVarDef("",j))
  methodList += (new JavaMethodDef(i.getName,i.getReturnType,parList.toList))
 }
 
val s= new JavaClassDef(
classname,
fieldList.toList,
methodList.toList,
0L,
sootclass.getInterfaces,
sootclass.getSuperclass)

List(s)
  }

}

