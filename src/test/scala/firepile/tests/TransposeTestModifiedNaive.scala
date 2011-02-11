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

var globalWorkSize = 2048
var localWorkSize =  128


def main(args: Array[String]) = { 

if (args.length > 0) 
   globalWorkSize = if (args.length > 0) (1 << args(0).toInt) else ( 1 << 20)
run
}
  
  def run = {
  
      val random = new Random(0)
      val idata = Array.fill( globalWorkSize ) (random.nextFloat)
      val odata= transpose(idata)(firepile.gpu)
      
      println("output")
      for ( i <- 0 until 200)
       println(" " +odata(i)) 
      
    
  }
  
  def transpose(idata : Array[Float])(implicit dev: Device): Array[Float] = {
  
      val space=dev.defaultPaddedPartition(idata.length)
      //dev.setWorkSizes(globalWorkSize, localWorkSize)
      val odata = new Array[Float](idata.length)
      val width = globalWorkSize
      val height = globalWorkSize
      
      //val n = idata.length
      
     space.spawn { 
        
        
        space.groups.foreach {
          g => {
           g.items.foreach {
	     item=> { 
         
            	       	        
                var xIndex = g.id(0)
	        var yIndex = g.id(1)
	      
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

