package firepile

import firepile._
import firepile.tree.Trees._
import firepile.Marshaling._
import java.util.ArrayList

class Item {
  var id: Int = 0
  def id(i: Int): Unit = { id = i }

}

class Group {

  val items: List[Item] = List(new Item)
  var id: Int = 0
  def id(i: Int): Unit = { id = i }
  var local: Int = 0
  def local(i: Int): Unit = { local = i }
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

  def spawn[A, B, C](block: => (A, B, C))(implicit m1: Manifest[A], m2: Manifest[B], m3: Manifest[C]): Unit = {
    println("in Spawn")
    val fvals = block
    val f = () => block

    println(" ::" + m1.toString + "::" + m2.toString + "::" + m3.toString)

    /*
val blockReducer: (A, B, C) => Unit = firepile.Compiler.compile {
      (A: Array[Int], B: Array[Int]) => block
    }
    //val B: Array[Int] = new Array[Int](blocks)

blockReducer(A, B, C)

*/

    val kernArgs = time(firepile.Compiler.findAllMethods(f, 3), "Compile")
    
    kernArgs match {
    
    case Some((kernName: String, treeList: List[Tree])) => {
						    val kernStr = new StringBuffer()
						       println(" name ::" + kernName + "::\n")
						      for (t: Tree <- treeList.reverse)
							kernStr.append(t.toCL)
					               }
                                                  
    case None => { println(" Something went wrong while creating Kernel!!!") }
	//compileNew(f._1,f._2,f._3,kernName,kernStr.toString)

     }
 }

  val groups: List[Group] = List(new Group)

}

