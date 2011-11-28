package firepile.compiler.util

import java.util.StringTokenizer
import java.io._

import soot.SootClass

import scala.tools.scalap._
import scala.tools.scalap.{ Main => Scalap }
import scala.tools.scalap.scalax.rules.scalasig._

import java.util.ArrayList
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import firepile.compiler.util.ClassDefs._

import scala.AnyRef
import scala.Seq
import scala.collection.mutable.HashMap

import ScalaTypeGen._
import JavaTypeGen._

object TypeGen {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("usage: TypeGen classname")
      sys.exit(1)
    }
/*    val cdlist = getSignature(args(0))

    cdlist match {
    
    case ClassDef => println("Class Def")
    case None => println(" None ")
    case _ => println(" !!!")
    }
    
  */
  }
  
 def getSignature( classname:  String, sootclass: SootClass) : List[ClassDef] = {
   val sig = if (classname.contains("$"))
     getScalaJavaSignature(classname, sootclass)
   else
     getScalaSignature(classname)

   if (sig == null) {
     println("SIG IS NULL")
     getJavaSignature(classname, sootclass)
   }
   else
     sig
 }

}

