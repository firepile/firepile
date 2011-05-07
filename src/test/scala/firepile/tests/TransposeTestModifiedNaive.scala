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

object TransposeTestModifiedNaive {

var height= 2048
var localWorkSize =  128


def main(args: Array[String]) = { 

if (args.length > 0) 
   height = if (args.length > 0) (1 << args(0).toInt) else ( 1 << 20)
run
}
  
  def run = {
  
      val random = new Random(0)
      // val idata = Array.fill( height * height ) (random.nextFloat)
      val idata    = BBArray.tabulate[Float](height * height)(i => random.nextFloat).directCopy
      val odata= transpose(idata, height,height )(firepile.gpu)
      
      //println("output")
      //for ( i <- 0 until odata.length)
       //println(" " +odata(i)) 
	   
	   Kernel.printTime
      
    
  }
  
  def transpose(idata : BBArray[Float], width : Int, height : Int)(implicit dev: Device): BBArray[Float] = {
  
      val space=dev.defaultPaddedPartition(idata.length)
	  val blockDim = 16
      val odata = new BBArray[Float](idata.length).directCopy
	  println(" inpu tlenght "+ idata.length)
	  val size =height - (height % blockDim) + blockDim 
      val globalArray = Array(size,size)
      val localArray =  Array(blockDim,blockDim)
      dev.setWorkSizes(globalArray, localArray)
	  Kernel.output("odata")
	  
   
     space.spawn { 
        
        space.groups.foreach {
          g => {
           g.items.foreach {
	     item=> { 
         
            	       	        
            var xIndex = item.globalId(0)
	        var yIndex = item.globalId(1)
	      
	        if( xIndex < width &&  yIndex < height ) 
	             {
	               val index_in = xIndex + width * yIndex
	               val index_out = yIndex + height * xIndex
	               
	               odata(index_out) = idata(index_in)
	             }
	         }
	      }
	    }
            
           }
          (odata,idata,height,width)
          }
      odata
   }
} 

