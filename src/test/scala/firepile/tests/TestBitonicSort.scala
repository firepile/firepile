package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.util.Unsigned._

import scala.collection.mutable.ArrayBuffer

object TestBitonicSort {
  val LOCAL_SIZE_LIMIT = 512
  val dir = 1.toUInt
  var arrayLength = 0.toUInt
  val numValues = 65536.toUInt

  def main(args: Array[String]) = {
    arrayLength = if (args.length > 0) (1 << args(0).toInt).toUInt else (1 << 20).toUInt

    println("arrayLength = " + arrayLength)

    val rand = new scala.util.Random(2009)

    val srcKey: Array[UInt]       = Array.fill(arrayLength.toInt)((rand.nextInt(numValues.toInt)).toUInt)
    val srcVal: Array[UInt]       = new Array[UInt](arrayLength.toInt)

    for (i <- 0 until arrayLength.toInt) srcVal(i) = i.toUInt

    val size = (2 * LOCAL_SIZE_LIMIT).toUInt
    val stride = (size / 2).toUInt

    val (keys_S, vals_S) = BitonicSort_S(srcKey, srcVal)
    println("sort completed")
   
    // val (keys_M, vals_M) = BitonicSort_M(keys_S, vals_S, arrayLength, size, stride, 1.toUInt)

    for (i <- 0 until 100)
      println("("+keys_S(i)+", "+vals_S(i)+")")
      
    println("done")
  }
    
  def BitonicSort_S(srcKey: Array[UInt], srcVal: Array[UInt]): (Array[UInt], Array[UInt]) = {
    implicit val gpu: Device = firepile.gpu

    val batch = arrayLength / 64

    gpu.setWorkSizes(batch * 64 / 2, LOCAL_SIZE_LIMIT / 2)
    gpu.setLocalMemSize(LOCAL_SIZE_LIMIT * 2)

    val bsSort: (Array[UInt], Array[UInt], Array[UInt]) => Unit = firepile.Compiler.compile {
      (srcKey: Array[UInt], srcVal: Array[UInt], dstKeyVal: Array[UInt]) => bitonicSortSort(srcKey, srcVal, dstKeyVal)
    }


    val outKeyVal: Array[UInt] = new Array[UInt](arrayLength.toInt * 2)

    // hardcoded globalWorkSize and localWorkSize similar to nvidia example
    
    bsSort(srcKey, srcVal, outKeyVal)

    // Puts are stored at even index numbers, calls are stored at odd index numbers
   
    val dstKey = new ArrayBuffer[UInt]()
    val dstVal = new ArrayBuffer[UInt]()

    for (n <- 0 until outKeyVal.length)
      if (n % 2 == 0) dstKey += outKeyVal(n)
      else dstVal += outKeyVal(n)

    (dstKey.toArray, dstVal.toArray)
  } 

  def BitonicSort_M(srcKey: Array[UInt], srcVal: Array[UInt], arrayLen: UInt, size: UInt, stride: UInt, sortDir: UInt): (Array[UInt], Array[UInt]) = {
    implicit val gpu: Device = firepile.gpu

    val batch = arrayLength / 64

    gpu.setWorkSizes(batch * 64 / 2, LOCAL_SIZE_LIMIT / 2)

    val bsMerge: (Array[UInt], Array[UInt], Array[UInt], Array[UInt], Array[UInt], Array[UInt], Array[UInt]) => Unit = firepile.Compiler.compile {
      (srcKey: Array[UInt], srcVal: Array[UInt], arrayLen: Array[UInt], size: Array[UInt], stride: Array[UInt], sortDir: Array[UInt], dstKeyVal: Array[UInt]) =>
        bitonicSortMerge(srcKey, srcVal, arrayLen, size, stride, sortDir, dstKeyVal)
      }


    val outKeyVal: Array[UInt] = new Array[UInt](arrayLength.toInt * 2)

    // hardcoded globalWorkSize and localWorkSize similar to nvidia example
    
    bsMerge(srcKey, srcVal, Array[UInt](arrayLen), Array[UInt](size), Array[UInt](stride), Array[UInt](sortDir), outKeyVal)

    // Puts are stored at even index numbers, calls are stored at odd index numbers
   
    val dstKey = new ArrayBuffer[UInt]()
    val dstVal = new ArrayBuffer[UInt]()

    for (n <- 0 until outKeyVal.length)
      if (n % 2 == 0) dstKey += outKeyVal(n)
      else dstVal += outKeyVal(n)

    (dstKey.toArray, dstVal.toArray)
  } 

  def bitonicSortSort(srcKey: Array[UInt], srcVal: Array[UInt], dstKeyVal: Array[UInt]) = (id: Id1, ldata: Array[UInt]) => {
    dstKeyVal(0) = srcVal(0)
    dstKeyVal(1) = srcVal(1)
    dstKeyVal(2) = srcVal(1002)
   
    val i = id.local
    /*
    val LOCAL_SIZE_LIMIT = 512.toUInt
    // Offset to beginning of subbatch and load data
    val groupIdUInt = (id.group.toInt).toUInt
    val localIdUInt = (id.local.toInt).toUInt

    val startPos = groupIdUInt * LOCAL_SIZE_LIMIT + localIdUInt 

    // local data stored as alternating key/value
    ldata((id.local + 0) * 2) = srcKey(startPos + 0)
    ldata((id.local + 0) * 2 + 1) = srcVal(startPos + 0)
    ldata((id.local + (LOCAL_SIZE_LIMIT / 2)) * 2) = srcKey(startPos + (LOCAL_SIZE_LIMIT / 2))
    ldata((id.local + (LOCAL_SIZE_LIMIT / 2)) * 2 + 1) = srcVal(startPos + (LOCAL_SIZE_LIMIT / 2))

    val comparatorI: UInt = (id.global & ((LOCAL_SIZE_LIMIT / 2) - 1)).toUInt

    
    var size: UInt = 2.toUInt
    while (size < LOCAL_SIZE_LIMIT) {
      val dirCond: UInt = if ( (comparatorI & (size / 2.toUInt).toUInt).toInt != 0 ) 1.toUInt else 0.toUInt
      var stride: UInt = (size / 2.toUInt).toUInt
      while (stride > 0.toUInt) {
        localMem.barrier
        val pos: UInt = (2.toUInt * localIdUInt - (localIdUInt & (stride - 1.toUInt).toUInt)).toUInt

        // ComparatorLocal inlined
        var keyA: UInt = ldata(pos * 2)
        var valA: UInt = ldata(pos * 2 + 1)
        var keyB: UInt = ldata((pos + stride) * 2)
        var valB: UInt = ldata((pos + stride) * 2 + 1)

        if ((keyA > keyB) == dirCond.toBoolean) {
          var t: UInt = keyA
          keyA = keyB
          keyB = t
          t = valA
          valA = valB
          valB = t
        }

        stride = stride >> 1
      }
      size = size << 1
    }
    

    // Odd/even arrys of LOCAL_SIZE_LIMIT elements sorted in opposite directions
    val dirCond: UInt = (groupIdUInt & 1.toUInt)
    var stride: UInt = (LOCAL_SIZE_LIMIT / 2.toUInt).toUInt
    while (stride > 0.toUInt) {
      localMem.barrier
      val pos: UInt = (2.toUInt * localIdUInt - (localIdUInt & (stride - 1.toUInt).toUInt)).toUInt

      // ComparatorLocal inlined
      var keyA: UInt = ldata(pos * 2)
      var valA: UInt = ldata(pos * 2 + 1)
      var keyB: UInt = ldata((pos + stride) * 2)
      var valB: UInt = ldata((pos + stride) * 2 + 1)

      if ((keyA > keyB) == dirCond.toBoolean) {
        var t: UInt = keyA
        keyA = keyB
        keyB = t
        t = valA
        valA = valB
        valB = t
      }

      stride = stride >> 1
    }
    

    localMem.barrier
  
    
    dstKeyVal(startPos * 2) = 1.toUInt // ldata(id.local * 2)
    dstKeyVal(startPos * 2 + 1) = 1.toUInt // ldata(id.local * 2 + 1)
    dstKeyVal((startPos + (LOCAL_SIZE_LIMIT / 2)) * 2) = 1.toUInt // ldata((id.local + (LOCAL_SIZE_LIMIT / 2)) * 2)
    dstKeyVal((startPos + (LOCAL_SIZE_LIMIT / 2)) * 2 + 1) = 1.toUInt // ldata((id.local + (LOCAL_SIZE_LIMIT / 2)) * 2 + 1)
 
    */


  }


  def bitonicSortMerge(srcKey: Array[UInt], srcVal: Array[UInt], arrayLenA: Array[UInt], sizeA: Array[UInt], strideA: Array[UInt], sortDirA: Array[UInt], dstKeyVal: Array[UInt]) = (id: Id1, ldata: Array[UInt]) => {
    val arrayLength: UInt = arrayLenA(0)
    val size = sizeA(0)
    val stride = strideA(0)
    val sortDir = sortDirA(0)

    val global_comparatorI: UInt = (id.global.toInt).toUInt
    val comparatorI: UInt = ((global_comparatorI & (arrayLength / 2.toUInt).toUInt) - 1.toUInt).toUInt // not sure about Order of Ops

    // Bitonic merge
    
    val dirCond: UInt = if ( (comparatorI & (size / 2.toUInt).toUInt).toInt != 0 ) 1.toUInt else 0.toUInt

    val dir: UInt = sortDir ^ dirCond
    val pos: UInt = 2.toUInt * (global_comparatorI - (global_comparatorI & (stride - 1.toUInt).toUInt)).toUInt

    var keyA = srcKey(pos + 0)
    var valA = srcVal(pos + 0)
    var keyB = srcKey(pos + stride.toInt)
    var valB = srcVal(pos + stride.toInt)

    // Comparator private
    if ((keyA > keyB) == dir.toBoolean) {
      var t: UInt = keyA
      keyA = keyB
      keyB = t

      t = valA
      valA = valB
      valB = t
    }

    dstKeyVal(pos * 2) = keyA
    dstKeyVal(pos * 2 + 1) = valA
    dstKeyVal((pos + stride) * 2) = keyB
    dstKeyVal((pos + stride) * 2 + 1) = valB
  }

}

