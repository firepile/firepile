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

object TypeGen {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("usage: ScalaTypeGen classname")
      exit(1)
    }
    val cdlist = getType(args(0))

    if (cdlist == null) {
      println("Class Def List is null")
      exit(1)
    }

  }
  
 def getType( classname:  String) : Option[ScalaClassDef] = {
 None
 }

}

