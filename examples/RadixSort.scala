object RadixSort {
  val WARP_SIZE = 32

  def main(args: Array[String]) = {
    var A = Array(123,432,654,3123,654,2123,543,131,653,123)

    radixSortUintHost(A, 4).foreach(i => println(i))
  }

  // LSB radix sort
  def radixSortUintHost(A: Array[Int], bits: Int): Array[Int] = {
    var a = A
    var b = new Array[Int](a.length)

    var rshift = 0
    var mask = ~(-1 << bits)

    while (mask != 0) {
      val cntArray = new Array[Int](1 << bits)

      for (p <- 0 until a.length) {
        var key = (a(p) & mask) >> rshift
        cntArray(key)+= 1
      }

      for (i <- 1 until cntArray.length)
        cntArray(i) += cntArray(i-1)

      for (p <- a.length-1 to 0 by -1) {
        var key = (a(p) & mask) >> rshift
        cntArray(key)-= 1
        b(cntArray(key)) = a(p)
      }

      val temp = b
      b = a
      a = temp

      mask <<= bits
      rshift += bits
    }

    b
  }

  // KERNEL
  def radixSortBlocksKeysOnly(/* global */ keysIn: Array[Tuple4[Int,Int,Int,Int]],
                              /* global */ keysOut: Array[Tuple4[Int,Int,Int,Int]],
                              nbits: Int,
                              startbit: Int,
                              numElements: Int,
                              totalBlock: Int)
                             (id: Id, localMem: LocalMem)
  {
    var key = keysIn(id.global)

    localMem.barrier

    radixSortBlockKeysOnly(/* by ref */ key, nbits, startbit)(id, localMem)

    keysOut(id.global) = key
  }

  def radixSortBlockKeysOnly(key: Tuple4[Int,Int,Int,Int], nbits: Int, startbit: Int)(id: Id, localMem: LocalMem) = {

    var shift = startbit

    while(shift < (startbit + nbits)) {
      val lsb = ((key._1 >> shift) & 0x1,
                 (key._2 >> shift) & 0x1,
                 (key._3 >> shift) & 0x1,
                 (key._4 >> shift) & 0x1)

      val r = rank4(lsb)(id, localMem)

      localMem((r._1 & 3) * id.localSize + (r._1 >> 2)) = key._1
      localMem((r._2 & 3) * id.localSize + (r._2 >> 2)) = key._2
      localMem((r._3 & 3) * id.localSize + (r._3 >> 2)) = key._3
      localMem((r._4 & 3) * id.localSize + (r._4 >> 2)) = key._4

      localMem.barrier

      // The above allows us to read without 4-way bank conflicts
      key._1 = localMem(id.local)
      key._2 = localMem(id.local + id.localSize)
      key._3 = localMem(id.local + 2 * id.localSize)
      key._4 = localMem(id.local + 3 * id.localSize)

      localMem.barrier

      shift += 1
    }
  }

  def rank4(preds: Tuple4[Int,Int,Int,Int])(id: Id, localMem: LocalMem): Tuple4[Int,Int,Int,Int] = {
    val address = scan4(preds)(id, localMem)

    /* local */ val numtrue = new Array[Int](1)

    if(id.local == id.localSize - 1)
      numtrue(0) = address._4 + preds._4

    localMem.barrier

    val idx = id.local * 4
    val rank = ((if (preds._1) address._1 else numtrue(0) + idx - address_.1),
                (if (preds._2) address._2 else numtrue(0) + idx + 1 - address_.2),
                (if (preds._3) address._3 else numtrue(0) + idx + 2 - address_.3),
                (if (preds._4) address._4 else numtrue(0) + idx + 3 - address_.4))
                
    rank
  }

//----------------------------------------------------------------------------
// scan4 scans 4*RadixSort::CTA_SIZE (???) numElements in a block (4 per thread), using 
// a warp-scan algorithm
//----------------------------------------------------------------------------
  
  def scan4(idata: Tuple4[Int,Int,Int,Int])(id: Id, localMem: LocalMem): Tuple4[Int,Int,Int,Int] = {
    val idx = id.local

    val val4 = idata
    val sum = new Array[Int](3)

    sum(0) = val4._1
    sum(1) = val4._2 + sum(0)
    sum(2) = val4._3 + sum(1)

    var vall = val4._4 = sum(2)

    vall = scanwarp(vall, localMem, 4)
    localMem.barrier

    if ((idx & (WARP_SIZE - 1)) == WARP_SIZE - 1)
      localMem(idx >> 5) = vall + val4._4 + sum(2)

    localMem.barrier

    vall += localMem(idx >> 5)

    val4._1 = vall
    val4._2 = vall + sum(0)
    val4._3 = vall + sum(1)
    val4._4 = vall + sum(2)

    val4
  }

  //----------------------------------------------------------------------------
  // Scans each warp in parallel ("warp-scan"), one element per thread.
  // uses 2 numElements of shared memory per thread (64 = elements per warp)
  //----------------------------------------------------------------------------
  def scanwarp(vall: Int, /* volatile local */ sData: Array[Int], maxlevel: Int): Int = {
    // The following is the same as 2 * RadixSort::WARP_SIZE * warpId + threadInWarp = 
    // 64*(threadIdx.x >> 5) + (threadIdx.x & (RadixSort::WARP_SIZE - 1))

    val idx = 2 * id.local - (id.local & (WARP_SIZE - 1))
    sData(idx) = 0
    idx += WARP_SIZE
    sData(idx) = vall

    if (0 <= maxlevel) sData(idx) += sData(idx - 1)
    if (1 <= maxlevel) sData(idx) += sData(idx - 2)
    if (2 <= maxlevel) sData(idx) += sData(idx - 4)
    if (3 <= maxlevel) sData(idx) += sData(idx - 8)
    if (4 <= maxlevel) sData(idx) += sData(idx - 16)

    sData(idx) - vall    // convert inclusize -> exclusive
  }

}

