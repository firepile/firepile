package firepile

import firepile._
import firepile.tree.Trees._
import firepile.Marshaling._
import java.util.ArrayList

class Item {
  var id: Int = 0
  def id(i: Int): Int = { id = i; 0 }
  val globalId: Int = 0

}

class Group {

  val items: List[Item] = List(new Item)
  var id: Int = 0
  def id(i: Int): Int = { id = i; 0 }
  var local: Int = 0
  def local(i: Int): Int = { local = i; 0 }
  def barrier = ()
  var size: Int = 0
  def size(i: Int): Int = { size =i ; size}

}

class Space(t: Int, b: Int) {

  val threads: Int = t
  val blocks: Int = b

  /*
def spawn[A1,A2](f: (A1,A2) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], dev: Device): Unit = {

val transA1 = implicitly[Marshal[A1]]
val transA2 = implicitly[Marshal[A2]]
val sizeA1 = transA1.sizes(1).head
val sizeA2 = transA2.sizes(1).head


}
*/

  def spawn[A, B, C](block: => (A, B, C))(implicit m1: Marshal[A], m2: Marshal[B], m3: Marshal[C], dev: Device) = {
    //println("in Spawn")
    val fvals = block
    val f = () => block

    //println(" ::" + m1.toString + "::" + m2.toString + "::" + m3.toString)

    val kernArgs = time(firepile.Compiler.findAllMethods(f, 3, List(m1, m2, m3)), "Compile")
    
    kernArgs match {
    
    case Some((kernName: String, treeList: List[Tree])) => {
						       val kernStr = new StringBuffer()
						       println(" name ::" + kernName + "::\n")
						       for (t: Tree <- treeList.reverse)
							kernStr.append(t.toCL)
							
						       firepile.Compiler.compileNew(fvals,kernName,kernStr.toString, dev) //(m1,m2,m3,dev) 
						       Kernel.printTime
						      }
                                                  
    case None => { println(" Something went wrong while creating Kernel!!!") }
	//

     }
 }
 
   def spawn[A, B, C, D](block: => (A, B, C, D))(implicit m1: Marshal[A], m2: Marshal[B], m3: Marshal[C], m4: Marshal[D], dev: Device) = {
     //println("in Spawn")
     val fvals = block
     val f = () => block
 
     //println(" ::" + m1.toString + "::" + m2.toString + "::" + m3.toString)
 
     val kernArgs = time(firepile.Compiler.findAllMethods(f, 4, List(m1, m2, m3, m4)), "Compile")
     
     kernArgs match {
     
     case Some((kernName: String, treeList: List[Tree])) => {
 						       val kernStr = new StringBuffer()
 						       println(" name ::" + kernName + "::\n")
 						       for (t: Tree <- treeList.reverse)
 							kernStr.append(t.toCL)
 							
 						        firepile.Compiler.compileNew(fvals,kernName,kernStr.toString,dev)(m1,m2,m3,m4) 
 						      }
                                                   
     case None => { println(" Something went wrong while creating Kernel!!!") }
 	//
 
      }
 }

   def spawn[A, B, C, D, E](block: => (A, B, C, D, E))(implicit m1: Marshal[A], m2: Marshal[B], m3: Marshal[C], m4: Marshal[D], m5: Marshal[E], dev: Device) = {
    println("in Spawn")
    val fvals = block
    println("fvals = block")
    val f = () => block
    println("f = () => block")

    //println(" ::" + m1.toString + "::" + m2.toString + "::" + m3.toString)

    val kernArgs = time(firepile.Compiler.findAllMethods(f, 5, List(m1, m2, m3, m4, m5)), "Compile")
    println("findAllmethods done")
    
    kernArgs match {
    
    case Some((kernName: String, treeList: List[Tree])) => {
						       val kernStr = new StringBuffer()
						       println(" name ::" + kernName + "::\n")
						       for (t: Tree <- treeList.reverse)
							kernStr.append(t.toCL)
							
						        firepile.Compiler.compileNew(fvals,kernName,kernStr.toString,dev)(m1,m2,m3,m4,m5) 
						      }
                                                  
    case None => { println(" Something went wrong while creating Kernel!!!") }
	//

     }
 }

  class GroupIterator extends Iterator[Group] {
    def hasNext = false
    def next = null
  }

  class GroupIterable extends Iterable[Group] {
    def iterator = new GroupIterator
    override def foreach[Unit](f: (Group) => Unit) = { } 
  }

  // val groups: List[Group] = List(new Group)
  val groups: GroupIterator = new GroupIterator

}

