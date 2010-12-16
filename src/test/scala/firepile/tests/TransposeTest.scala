package firepile.tests

import firepile._
import firepile.Device
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
import scala.math.{ceil, pow, log}
import firepile.util.Unsigned._

object TransposeTest {

val localWorkSize: Int =  16              // 1D var for # of work items in the work group	
val globalWorkSize: Int = 2048
val maxThreads: Int = 512 
val maxBlocks: Int = 16

def main(args: Array[String]) = run
  
  def run = {
      
      val random = new Random(0)
      val idata = Array.fill( globalWorkSize * globalWorkSize) (random.nextFloat)
      
      val odata= transpose(idata)
      
    
  }
  
  def transpose(idata : Array[Float]): Array[Float] = {
      implicit val gpu: Device = firepile.gpu
      gpu.setWorkSizes(globalWorkSize, localWorkSize)
      val threads = (if (idata.length < gpu.maxThreads*2) pow(2, ceil(log(idata.length) / log(2))) else gpu.maxThreads).toInt
      val blocks = ((idata.length + (threads * 2 - 1)) / (threads * 2)).toInt

      val transposeGenerator : (Array[Float],Array[Float]) => Unit = firepile.Compiler.compile {
        (A: Array[Float], B: Array[Float]) => matrixTranspose_naive(A, B)
      }
      
       
      val odata : Array[Float] = new Array[Float](globalWorkSize)
      transposeGenerator(idata,odata)
      odata
 }
 
 def matrixTranspose_naive(odata : Array[Float], idata: Array[Float]) =
 (id: Id2, block: Array[Float]) => {
     
     val width= 2048
     val height= 2048
     val BLOCK_DIM = 16
     var (xIndex, yIndex)  = point22int(id.global)
    // var yIndex = id.group //get_global_id(1)
     
     if (xIndex  < width && yIndex < height)
     {
         val index_in  = xIndex  + width * yIndex
         val index_out = yIndex + height * xIndex
         odata(index_out) = idata(index_in)
     }
}

/*
 
def matrixTranpose( odata : Array[Float], idata: Array[Float]) = 
 (id: Id2, block: Array[Float]) => {
 
        //val offset=
        var (xIndex, yIndex)  = id.global.group
       // var yIndex = id.group.toInt //get_global_id(1)
        val xIndexLocal  = id.local.toInt
        val yIndexLocal = id.local.toInt //get_local_id(1)
        
        val width= 2048
        val height= 2048
        val BLOCK_DIM = 16
	// read the matrix tile into shared memory
	
	if((xIndex < width) && (yIndex < height))
	{
		val index_in = yIndex * width + xIndex 
		block(yIndexLocal*(BLOCK_DIM+1)+xIndexLocal) = idata(index_in)
	}

	localMem.barrier

	// write the transposed matrix tile to global memory
	xIndex = xIndex * BLOCK_DIM + xIndexLocal
	yIndex = yIndex * BLOCK_DIM + yIndexLocal
	if((xIndex < height) && (yIndex < width))
    {
		val index_out = yIndex * height + xIndex
		odata(index_out) = block(xIndexLocal*(BLOCK_DIM+1)+yIndexLocal)
	}
}
*/

}