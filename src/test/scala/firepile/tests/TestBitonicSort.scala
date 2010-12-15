package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.util.Unsigned._

import scala.collection.mutable.ArrayBuffer

object TestBitonicSort {
  def main(args: Array[String]) = {
    val arrayLength = if (args.length > 0) scala.math.pow(2, args(0).toInt).toInt else scala.math.pow(2, 10).toInt 

    val rand = new scala.util.Random(2009)

    val srcKey: Array[UInt]       = Array.fill(arrayLength)((rand.nextInt).toUInt)
    val srcVal: Array[UInt]       = Array.fill(arrayLength)((rand.nextInt).toUInt)



    BitonicSort(srcKey, srcVal, srcKey.length.toUInt, 4.toUInt, 16.toUInt, 0.toUInt)
      
    println("done")
  }
    
  def BitonicSort(srcKey: Array[UInt], srcVal: Array[UInt], arrayLen: UInt, size: UInt, stride: UInt, sortDir: UInt): (Array[UInt], Array[UInt]) = {
    implicit val gpu: Device = firepile.gpu
    gpu.setWorkSizes(60 * 1024, 128)

    val bs: (Array[UInt], Array[UInt], Array[UInt], Array[UInt], Array[UInt], Array[UInt], Array[UInt]) => Unit = firepile.Compiler.compile {
      (srcKey: Array[UInt], srcVal: Array[UInt], arrayLen: Array[UInt], size: Array[UInt], stride: Array[UInt], sortDir: Array[UInt], dstKeyVal: Array[UInt]) =>
        bitonicSortK(srcKey, srcVal, arrayLen, size, stride, sortDir, dstKeyVal)
      }

    val outKeyVal = new Array[UInt](srcKey.length*2)

    // hardcoded globalWorkSize and localWorkSize similar to nvidia example
    
    //bs(srcKey, srcVal, Array[UInt](arrayLen), Array[UInt](size), Array[UInt](stride), Array[UInt](sortDir), outKeyVal)

    // Puts are stored at even index numbers, calls are stored at odd index numbers
   
    val dstKey = new ArrayBuffer[UInt]()
    val dstVal = new ArrayBuffer[UInt]()

    for (n <- 0 until outKeyVal.length)
      if (n % 2 == 0) dstKey += outKeyVal(n)
      else dstVal += outKeyVal(n)

    (dstKey.toArray, dstVal.toArray)
  } 

  def bitonicSortK(srcKey: Array[UInt], srcVal: Array[UInt], arrayLenA: Array[UInt], sizeA: Array[UInt], strideA: Array[UInt], sortDirA: Array[UInt], dstKeyVal: Array[UInt]) = (id: Id1, ldata: Array[UInt]) => {
    val arrayLength: UInt = arrayLenA(0)
    val size = sizeA(0)
    val stride = strideA(0)
    val sortDir = sortDirA(0)

    val global_comparatorI: UInt = (id.global.toInt).toUInt
    val comparatorI: UInt = ((global_comparatorI & (arrayLength / 2.toUInt).toUInt) - 1.toUInt).toUInt // not sure about Order of Ops

    // Bitonic merge
    
    val dirCond = if ( (comparatorI & (size / 2.toUInt).toUInt) != 0.toUInt ) 1.toUInt else 0.toUInt

    val dir: UInt = sortDir ^ dirCond
    val pos: UInt = 2.toUInt * (global_comparatorI - (global_comparatorI & (stride - 1.toUInt).toUInt)).toUInt

    var keyA = srcKey(pos + 0)
    var valA = srcVal(pos + 0)
    var keyB = srcKey(pos + stride.toInt)
    var valB = srcVal(pos + stride.toInt)

    // Comparator private
    if ((keyA > keyB) == dir.toBoolean) {
      var t: UInt = 0.toUInt
      t = keyA
      keyA = keyB
      keyB = t

      t = valA
      valA = valB
      valB = t
    }

    dstKeyVal(pos * 2) = keyA
    dstKeyVal(pos * 2) = valA
    dstKeyVal((pos + stride) * 2) = keyB
    dstKeyVal((pos + stride) * 2 + 2) = valB
  }

}

