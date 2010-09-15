import firepile.util.Math

object MersenneTwister {
  case class mt_struct_stripped(matrix_a: UInt, mask_b: UInt, mask_c: UInt, seed: UInt)

  val MT_RND_COUNT = 4096
  val MT_MM = 9
  val MT_NN = 19
  val MT_WMASK = 0xFFFFFFFFU
  val MT_UMASK = 0xFFFFFFFEU
  val MT_LMASK = 0x1U
  val MT_SHIFT0 = 12
  val MT_SHIFTB = 7
  val MT_SHIFTC = 15
  val MT_SHIFT1 = 18
  val PI = 3.14159265358979f

  // KERNEL
  def mersenneTwister(/* global */ d_Rand: Array[Float], /*global */ d_MT: Array[mt_struct_stripped], rPerRng: Int)(id: Id, localMem: LocalMem) = {
    var (iState, iState1, iStateM, iOut) = (0, 0, 0, 0)
    var (mti, mti1, mtiM, x) = (0U, 0U, 0U, 0U)
    var mt: Array[UInt] = new Array(MT_NN)
    var (matrix_a, mask_b, mask_c) = (0U, 0U, 0U)

    matrix_a = d_MT(id.global).matrix_a
    mask_b = d_MT(id.global).mask_b
    mask_c = d_MT(id.global).mask_c

    // Initialize current state
    mt(0) = d_MT(id.global).seed
    for (iState <- 1 until MT_NN)
      mt(iState) = (1812433253U * (mt(iState - 1) ^ (mt(iState - 1) >> 30)) + iState) & MT_WMASK

    iState = 0
    mti1 = mt(0)

    for (iOut <- 0 until nPerRng) {
      iState1 = iState + 1
      iStateM = iState + MT_MM
      if (iState1 >= MT_NN) iState1 -= MT_NN
      if (iStateM >= MT_NN) iStateM -= MT_NN
      mti = mti1
      mti1 = mt(iState1)
      mtiM = mt(iStateM)

      // MT recurrence
      x = (mti & MT_UMASK) | (mti1 & MT_LMASK)
      x = mtiM ^ (x >> 1) ^ ((x & 1) ? matrix_a : 0)

      mt(iState) = x
      iState = iState1

      //Tempering transformation
      x ^= (x >> MT_SHIFT0)
      x ^= (x << MT_SHIFTB) & mask_b
      x ^= (x << MT_SHIFTC) & mask_c
      x ^= (x >> MT_SHIFT1)

      //Convert to (0, 1] float and write to global memory
      d_Rand(globalID + iOut * MT_RNG_COUNT) = ((x.toFloat) + 1.0f) / 4294967296.0f
    }
  }


////////////////////////////////////////////////////////////////////////////////
// Transform each of MT_RNG_COUNT lanes of nPerRng uniformly distributed
// random samples, produced by MersenneTwister(), to normally distributed lanes
// using Cartesian form of Box-Muller transformation.
// nPerRng must be even.
////////////////////////////////////////////////////////////////////////////////
  def boxMullerTrans(/*global*/ u1: Float, /*global*/ u2: Float): Tuple2[Float,Float] = {
    val r = Math.sqrt(-2.0f * Math.log(u1))
    val phi = 2 * PI * u2
    (r * Math.cos(phi), r * Math.sin(phi))
  }

  // KERNEL
  def boxMuller(/*global*/ d_Rand: Array[Float], nPerRng: Int) = { 
    for (iOut <- 0 until nPerRng by 2)
        boxMullerTrans(/*by ref*/ d_Rand(globalID + (iOut + 0) * MT_RNG_COUNT),
		       /*by ref*/ d_Rand(globalID + (iOut + 1) * MT_RNG_COUNT))
  }
}

