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

val globalWorkSize = 2048
val localWorkSize =  128

//szGlobalWorkSize[0] = sizePerGPU;
//szGlobalWorkSize[1] = shrRoundUp(BLOCK_DIM, size_y);

//szGlobalWorkSize[0] = sizePerGPU;
//szGlobalWorkSize[1] = shrRoundUp(BLOCK_DIM, size_y);

def main(args: Array[String]) = run
  
  def run = {
      
      
      val random = new Random(0)
      val idata = Array.fill( globalWorkSize * globalWorkSize) (random.nextFloat)
      //println("input")
      //for (i <- idata)
      //println(" " +i) 
      val odata= transpose(idata)(firepile.gpu)
      
      println("output")
      for ( i <- 0 until 200)
       println(" " +odata(i)) 
      
    
  }
  
  def transpose(idata : Array[Float])(implicit dev: Device): Array[Float] = {
  
      val space=dev.defaultPaddedPartition(idata.length)
      dev.setWorkSizes(globalWorkSize * globalWorkSize, localWorkSize)
      val odata = new Array[Float](idata.length)
      
      val n = idata.length
      
     space.spawn { 
        
        space.groups.foreach {
          g => {
           g.items.foreach {
	     item=> { 
         
         
                val width= 2048
	        val height= 2048
      	       	        
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
          (odata,idata,n)
          }
      odata
   }
} 

