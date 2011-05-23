package firepile

import java.util.ArrayList
import scala.collection.mutable.HashMap
import soot.{Type => SootType}
import java.io.{File, BufferedWriter, FileWriter, IOException}


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
try {
  val f = new File("output.csv")
  val bw = new BufferedWriter(new FileWriter(f, true))

for( i <- List("Copy to GPU", "GPU", "From GPU")){ 
val t = time.get(i)
t match {

case Some(l: Long) => {
  println(" Time for ::"+ i +"  is ::"+ l/1000000. +" Milli seconds ")
  

  bw.write((l/1000000.).toString + ",")
  }
  case _ => throw new RuntimeException("Key " + i + " not found in hashmap")
  }
 }
 
 bw.write('\n')
 bw.close

 } catch {
   case e: IOException => println("File not found")
   case _ => println("some other file problem")
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
var closureFName: String = null

}




