package firepile

import firepile._
import firepile.tree.Trees._
import firepile.Marshaling._
import java.util.ArrayList

class Item {
  var id: Int = 0
  def id(i: Int): Int = { id = i; 0 }

}

class Group {

  val items: List[Item] = List(new Item)
  var id: Int = 0
  def id(i: Int): Int = { id = i; 0 }
  var local: Int = 0
  def local(i: Int): Int = { local = i; 0 }
  def barrier = ()

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

    val kernArgs = time(firepile.Compiler.findAllMethods(f, 3), "Compile")
    
    kernArgs match {
    
    case Some((kernName: String, treeList: List[Tree])) => {
						       val kernStr = new StringBuffer()
						       println(" name ::" + kernName + "::\n")
						       for (t: Tree <- treeList.reverse)
							kernStr.append(t.toCL)
							
						        firepile.Compiler.compileNew(fvals._1,fvals._2,fvals._3,kernName,kernStr.toString)(m1,m2,m3,dev) 
						      }
                                                  
    case None => { println(" Something went wrong while creating Kernel!!!") }
	//

     }
 }

  val groups: List[Group] = List(new Group)

}

