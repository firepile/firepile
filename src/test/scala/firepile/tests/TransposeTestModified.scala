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

object TransposeTestModified {

val globalWorkSize = 2048
val localWorkSize =  128

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
          
            val block = Array.ofDim[Float](g.items.size)    

	   g.items.foreach {
	     item=> { 
         
         
                val width= 2048
	        val height= 2048
      	        val BLOCK_DIM = 16
      	        
                var xIndex = g.id(0)
	        var yIndex = g.id(1)
	        val xIndexLocal  =  item.id(0)
                val yIndexLocal =   item.id(1)
         
	      if(xIndex < width){
	         if(yIndex < height)
        		{
			val index_in = yIndex * width + xIndex 
			block(yIndexLocal*(BLOCK_DIM+1)+xIndexLocal) = idata(index_in)
        		}
                }
		g.barrier

		// write the transposed matrix tile to global memory
		xIndex = xIndex * BLOCK_DIM + xIndexLocal
		yIndex = yIndex * BLOCK_DIM + yIndexLocal
		if(xIndex < height)
		   {
		       if(yIndex < width) {
			val index_out = yIndex * height + xIndex
			odata(index_out) = block(xIndexLocal*(BLOCK_DIM+1)+yIndexLocal)
		       }
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