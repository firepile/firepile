package firepile.tests

import firepile._
import firepile.Device
import firepile.Space
import firepile.Arrays._
import firepile.Spaces._
import firepile.util.BufferBackedArray._
import firepile.tree.Trees.Tree
import com.nativelibs4java.opencl._
import com.nativelibs4java.util._
import java.nio.FloatBuffer
import java.nio.ByteOrder
import scala.collection.JavaConversions._
import firepile.Marshaling._
import scala.util.Random

object VecAdd {
var n = 4096
def main(args: Array[String]) = { 

if (args.length > 0) 
   n = if (args.length > 0) (1 << args(0).toInt) else ( 1 << 20)
run
}
  
  def run = {
  
      val random = new Random(0)
      // val idata = Array.fill( height * height ) (random.nextFloat)
      val idata1    = BBArray.tabulate[Float](n)(i => random.nextFloat)
	  val idata2    = BBArray.tabulate[Float](n)(i => random.nextFloat)
	  
      val odata= VecAdd(idata1,idata2)(firepile.gpu)
      
      println("output")
	  Kernel.printTime
      //for ( i <- 0 until odata.length)
       //println(" " +odata(i)) 
      
    
  }
  
  def VecAdd(idata1 : BBArray[Float], idata2 : BBArray[Float])(implicit dev: Device): BBArray[Float] = {
  
      val space=dev.defaultPaddedPartition(idata1.length)
	  val odata = new BBArray[Float](idata1.length)
	  //dev.setWorkSizes(globalArray, localArray)
	  Kernel.output("odata")
	  
	 val n = idata1.length
   
     space.spawn { 
        
        space.groups.foreach {
          g => {
           g.items.foreach {
	     item=> { 
         
            val iGID = item.globalId

            if (iGID < n)
                odata(iGID) = idata1(iGID) + idata2(iGID)  	        
                  
            }
	      }
	    }
            
           }
          (odata,idata1,idata2,n)
          }
      odata
   }
} 

