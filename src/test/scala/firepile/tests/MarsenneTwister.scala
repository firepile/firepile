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

object MarsenneTwister {

val MT_RNG_COUNT=4096
val MT_MM=9
val MT_NN=19
val MT_WMASK: Short =0xFFFFFFFF
val MT_UMASK: Short =0xFFFFFFFE
val MT_LMASK: Short =0x1
val MT_SHIFT0=12
val MT_SHIFTB=7
val MT_SHIFTC=15
val MT_SHIFT1=18
val PI=3.14159265358979f

val globalWorkSize= MT_RNG_COUNT      // 1D var for Total # of work items
val localWorkSize =  128                // 1D var for # of work items in the work group	
val seed = 777
val nPerRng = 5860                      // # of recurrence steps, must be even if do Box-Muller transformation
val nRand = MT_RNG_COUNT * nPerRng  

//val NUM_ITEMS = 16384 // 1048576
val maxThreads = 512 
val maxBlocks = 64

  def main(args: Array[String]) = run
  
  def run = {
      val random = new Random(0)
      val randInput1 = (Array.fill(globalWorkSize)(random.nextLong.toUInt))
      val randInput2 = (Array.fill(globalWorkSize)(random.nextLong.toUInt))
      val randInput3 = (Array.fill(globalWorkSize)(random.nextLong.toUInt))
      val randInput4 = (Array.fill(globalWorkSize)(random.nextLong.toUInt))
      
      val output= RandomNumGen(randInput1,randInput2,randInput3,randInput4,nPerRng)
      
      for(i <- output)
      println(":"+i+":")
    
  }
  
  /*
  void loadMTGPU(const char *fname, const unsigned int seed, mt_struct_stripped *h_MT, const size_t size)
  {
      FILE* fd = 0;
         
      for (unsigned int i = 0; i < size; i++)
          fread(&h_MT[i], sizeof(mt_struct_stripped), 1, fd);
      fclose(fd);
  
      for(unsigned int i = 0; i < size; i++)
          h_MT[i].seed = seed;
}
  */
  
  def RandomNumGen(matrix_a : Array[UInt], mask_a: Array[UInt], mask_b: Array[UInt], seed : Array[UInt], n: Int): Array[Float] = {
    implicit val gpu: Device = firepile.gpu
    gpu.setWorkSizes(globalWorkSize, localWorkSize)
    val threads = (if (matrix_a.length < gpu.maxThreads*2) pow(2, ceil(log(matrix_a.length) / log(2))) else gpu.maxThreads).toInt
    val blocks = ((matrix_a.length + (threads * 2 - 1)) / (threads * 2)).toInt
   
    // val add: (Float, Float) => Float = _+_

    val RandomNumberGenerator : (Array[UInt], Array[UInt], Array[UInt], Array[UInt], Int, Array[Float]) => Unit = firepile.Compiler.compile {
      (A: Array[UInt], B: Array[UInt], C: Array[UInt], D: Array[UInt], nPerRng: Int,E: Array[Float]) => MersenneTwister(A, B, C,D, nPerRng,E)
    }
    val d_Rand : Array[Float] = new Array[Float](blocks)
    //val F: Array[UInt] = new Array[UInt](MT_NN)
    RandomNumberGenerator(matrix_a, mask_a, mask_b,seed, nPerRng,d_Rand)
    d_Rand
 }
  
////////////////////////////////////////////////////////////////////////////////
// OpenCL Kernel for Mersenne Twister RNG
////////////////////////////////////////////////////////////////////////////////
def MersenneTwister(matrix_a: Array[UInt], mask_b: Array[UInt], mask_c: Array[UInt],seed: Array[UInt], nPerRng: Int,d_Rand: Array[Float]) = 
 (id: Id1, mt: Array[UInt]) => {
    
    val i = id.group
    var iState: Int = 0
    var iState1: Int = 0
    var iStateM: Int = 0
    var iOut: Int = 0
    var mti: UInt = 0.toUInt
    var mti1: UInt = 0.toUInt
    var mtiM: UInt = 0.toUInt
    var x : UInt = 0.toUInt
    //var mt =new Array[UInt](MT_NN)
    var m_a: UInt = 0.toUInt
    var m_b: UInt = 0.toUInt
    var m_c: UInt = 0.toUInt
    var cond: UInt =0.toUInt

    //Load bit-vector Mersenne Twister parameters
    m_a   = matrix_a(i)
    m_b   = mask_b(i)
    m_c   = mask_c(i)
        
    //Initialize current state
    mt(0) = seed(i)
        iState = 1
        while(iState < MT_NN) {
        mt(iState) = ((1812433253.toShort * (mt(iState - 1) ^ (mt(iState - 1) >> 30)) + iState) & MT_WMASK).toUInt;
        iState+=1
        }

    iState = 0
    mti1 = mt(0)

    iOut = 0
    while(iOut < nPerRng) {
        iState1 = iState + 1
        iStateM = iState + MT_MM
        if(iState1 >= MT_NN) iState1 -= MT_NN
        if(iStateM >= MT_NN) iStateM -= MT_NN
        mti  = mti1
        mti1 = mt(iState1)
        mtiM = mt(iStateM)

	    // MT recurrence
        x = (mti & MT_UMASK.toUInt) | (mti1 & MT_LMASK.toUInt)
        cond = x & 1.toUInt
            if(cond>0.toUInt)        
	    x = mtiM ^ (x >> 1.toUInt) ^ ( m_a)
	    else
	    x = mtiM ^ (x >> 1.toUInt) ^ (0.toUInt)

        mt(iState) = x
        iState = iState1

        //Tempering transformation
        x ^= (x >> MT_SHIFT0)
        x ^= (x << MT_SHIFTB) & m_b
        x ^= (x << MT_SHIFTC) & m_c
        x ^= (x >> MT_SHIFT1)

        //Convert to (0, 1] float and write to global memory
        d_Rand(i + iOut * MT_RNG_COUNT) = (x.toFloat + 1.0f) / 4294967296.0f;
        iOut+=1
    }
  }
  
  
  /*//////////////////////////////////////////////////////////////////////////////
  // Transform each of MT_RNG_COUNT lanes of nPerRng uniformly distributed
  // random samples, produced by MersenneTwister(), to normally distributed lanes
  // using Cartesian form of Box-Muller transformation.
  // nPerRng must be even.
  //////////////////////////////////////////////////////////////////////////////
  
  void BoxMullerTrans(__global float *u1, __global float *u2)
  {
      float   r = sqrt(-2.0f * log(*u1));
      float phi = 2 * PI * (*u2);
      *u1 = r * native_cos(phi);
      *u2 = r * native_sin(phi);
  }
  
  def BoxMuller( d_Rand: Float, nPerRng: Int) 
  {
      int i = id.group
  
      for (int iOut = 0; ; iOut += 2)
          while(iOut < nPerRng) {
          BoxMullerTrans(&d_Rand[globalID + (iOut + 0) * MT_RNG_COUNT],&d_Rand[globalID + (iOut + 1) * MT_RNG_COUNT])
          }
  }
*/

}