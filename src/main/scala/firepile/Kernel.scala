package firepile

import java.util.ArrayList
import scala.collection.mutable.HashMap
import soot.{Type => SootType}


object Kernel {

def apply(t: Int, b: Int) = {

threads= t
blocks = b
}

def output(name: String) = {
outputArgs.add(name)
}

def setTime(label: String, newTime : Long): Unit = {

val t = time.get(label)
t match {

case Some(l: Long) => time.put(label,newTime+l)
case _ => time.put(label,newTime)
 }
}

def printTime() = {

for( i <- time.keys){ 
val t = time.get(i)
t match {

case Some(l: Long) => println(" Time for ::"+ i +"  is ::"+ l/1000000000. +" seconds ")
case _ => { } 
  }
 }
}

var threads: Int =0
var blocks: Int =0
var level: Int = 1
val src: String = ""
val globalArgs = new ArrayList[(String,SootType,Int)]()
val localArgs = new ArrayList[(String,SootType,Int)]()
val outputArgs = new ArrayList[String]()
val time = new HashMap[String,Long]()

}




