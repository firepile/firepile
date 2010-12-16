package firepile.tests

import firepile.util.Unsigned._

object CPUMersenneTwister {

val MT_RNG_COUNT=4096
val seed: UInt = 777.toUInt
val nPerRng: Int = 5860                      
val nRand = MT_RNG_COUNT * nPerRng  

def cpuMersenneTwister(matrix_a: Array[UInt], mask_b: Array[UInt], mask_c: Array[UInt]): Array[Float] = {
    
    var d_Rand = new Array[Float](nRand)
    
    for ( i <-0 until MT_RNG_COUNT) {
    
    //val MT_RNG_COUNT: Int=4096
    val MT_MM: Int=9
    val MT_NN: Int=19
    val MT_WMASK: Short =0xFFFFFFFF
    val MT_UMASK: Short =0xFFFFFFFE
    val MT_LMASK: Short =0x1
    val MT_SHIFT0: Int=12
    val MT_SHIFTB: Int=7
    val MT_SHIFTC: Int=15
    val MT_SHIFT1: Int =18
    val PI=3.14159265358979f
    //val nPerRng:Int = 5860 
    val localSeed: UInt = 777.toUInt
    val mt = new Array[UInt](MT_NN)
    
    var iState: Int = 0
    var iState1: Int = 0
    var iStateM: Int = 0
    var iOut: Int = 0
    var mti: UInt = iOut.toUInt
    var mti1: UInt = mti
    var mtiM: UInt = mti
    var x : UInt = mti
    //var mt =new Array[UInt](MT_NN)
    var m_a: UInt = mti
    var m_b: UInt = mti
    var m_c: UInt = mti
    var cond: UInt = mti
   
    var something: UInt = ((mti & ( mti1 / 2.toUInt ).toUInt) - 1.toUInt).toUInt
    //Load bit-vector Mersenne Twister parameters
    m_a   = matrix_a(i)
    m_b   = mask_b(i)
    m_c   = mask_c(i)
        
    //Initialize current state
    
    mt(0) = localSeed
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
        
        d_Rand(i * nPerRng + iOut) = (x.toFloat + 1.0f) / 4294967296.0f;
        iOut+=1
      }
    
    }
    
    d_Rand
  }


}