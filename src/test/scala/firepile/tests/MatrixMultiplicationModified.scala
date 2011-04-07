
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
var height = 100
val localWorkSize = 256
var globalWorkSize = 0

def main(args: Array[String]) = {
 
  height= if (args.length > 0) (args(0).toInt) else 100


  globalWorkSize = localWorkSize * height
  run
}  
  def run = {
      
      
      val random = new Random(0)
      //val idata1 = Array.fill( width * height) (random.nextFloat)
      //val idata2 = Array.fill( width ) (random.nextFloat)
      val idata1    = BBArray.tabulate[Float](width * height)(i => random.nextFloat)
      val idata2    = BBArray.tabulate[Float](width)(i => random.nextFloat)
      val odata= matrixMul(idata1,idata2,width,height)(firepile.gpu)
      
      println("output")
      for ( i <- 0 until odata.length)
       println(" " +odata(i)) 
      
    
  }
  //LocalworkSize = 256
  //GlobalWorkSize = LocalWorkSize * Height
  //unsigned int size = width * height;
  //    unsigned int mem_size_M = size * sizeof(float);
  //    M = (float*)malloc(mem_size_M);
  //    unsigned int mem_size_V = width * sizeof(float);
  //    V = (float*)malloc(mem_size_V);
  //  unsigned int mem_size_W = height * sizeof(float);
  
  def matrixMul(idata1 : BBArray[Float], idata2 : BBArray[Float], width: Int, height: Int)(implicit dev: Device): BBArray[Float] = {
  
      val space=dev.defaultPaddedPartition(idata1.length)
      dev.setWorkSizes(globalWorkSize, localWorkSize)
      
      //val width  = 1100
      //val height = 100
      val n = idata1.length
      val odata = new BBArray[Float](height)
      
     space.spawn { 
        
        space.groups.foreach {
          g => {
          
           g.items.foreach {
	     item=> { 
         
           
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
          (odata,idata1,idata2,width,height)
          }
      odata
   }
} 
