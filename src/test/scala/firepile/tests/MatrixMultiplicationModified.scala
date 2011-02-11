
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

object MatrixMultiplicationModified {


val width  = 1100
val height = 100000
val localWorkSize = 256
val globalWorkSize = localWorkSize * height

def main(args: Array[String]) = run
  
  def run = {
      
      
      val random = new Random(0)
      val idata1 = Array.fill( width * height) (random.nextFloat)
      val idata2 = Array.fill( width ) (random.nextFloat)
      val odata= transpose(idata1,idata2)(firepile.gpu)
      
      println("output")
      for ( i <- 0 until 200)
       println(" " +odata(i)) 
      
    
  }

  def transpose(idata1 : Array[Float], idata2 : Array[Float])(implicit dev: Device): Array[Float] = {
  
      val space=dev.defaultPaddedPartition(idata1.length)
      dev.setWorkSizes(localWorkSize, globalWorkSize)
      val odata = new Array[Float](height)
            
      val n = idata1.length
      
     space.spawn { 
        
        space.groups.foreach {
          g => {
          
           g.items.foreach {
	     item=> { 
         
                val width  = 1100
                val height = 100000
                val y = g.id
                  if( y < height ) {
                var dotProduct = 0f
                  
                var x = 0
                   while(x < width ) {
                   
                     dotProduct+= idata1((y * width) + x ) * idata2(x)
                     x+=1
                    }
                 odata(y) = dotProduct
                  }
		}
	      }
	    }
            
           }
          (odata,idata1,idata2)
          }
      odata
   }
} 


__kernel void MatVecMulUncoalesced0(const __global float* M,
                                    const __global float* V,
                                    uint width, uint height,
                                    __global float* W)
{
    // Row index
    uint y = get_global_id(0);
    if (y < height) {
    
        // Row pointer
        const __global float* row = M + y * width;

        // Compute dot product  
        float dotProduct = 0;
        for (int x = 0; x < width; ++x)
            dotProduct += row[x] * V[x];

        // Write result to global memory
        W[y] = dotProduct;
    }
}
*/