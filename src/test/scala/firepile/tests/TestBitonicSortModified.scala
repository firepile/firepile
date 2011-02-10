package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.util.Unsigned._

import com.nativelibs4java.opencl.CLByteBuffer
import com.nativelibs4java.opencl.CLMem

import scala.collection.mutable.ArrayBuffer

object TestBitonicSortModified {
  val LOCAL_SIZE_LIMIT = 1024
  val dir = 1.toUInt
  var arrayLength = 0.toUInt
  val numValues = 65536.toUInt

  def main(args: Array[String]) = {
    arrayLength = if (args.length > 0) (1 << args(0).toInt).toUInt else (1 << 20).toUInt
    implicit val gpu: Device = firepile.gpu

    println("arrayLength = " + arrayLength)

    val rand = new scala.util.Random(2009)

    val srcKey: Array[UInt]       = Array.fill(arrayLength.toInt)((rand.nextInt(numValues.toInt)).toUInt)
    val srcVal: Array[UInt]       = new Array[UInt](arrayLength.toInt)

    for (i <- 0 until arrayLength.toInt) srcVal(i) = i.toUInt

    val (keys_S, vals_S) = runBitonic(srcKey, srcVal)

    println("Sort completed")


    for (i <- 0 until arrayLength.toInt)
      println("("+keys_S(i)+", "+vals_S(i)+")")
      
    println("done")
  }

  def runBitonic[A1,A2](a1: A1, a2: A2)(implicit ma1: Marshal[A1], ma2: Marshal[A2], dev: Device): (Array[UInt], Array[UInt]) = {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val arrayLen = transA1.sizes(a1).head / sizeA1
    val kernStrSort = new StringBuffer()
    val kernStrMerge = new StringBuffer()

    val dstKeyOut: Array[UInt] = Array.fill(arrayLen)(new UInt(0))
    val dstValOut: Array[UInt] = Array.fill(arrayLen)(new UInt(0))
    val bufKeyOut = dev.context.createByteBuffer(CLMem.Usage.Output, arrayLen * sizeA1)
    val bufValOut = dev.context.createByteBuffer(CLMem.Usage.Output, arrayLen * sizeA2)

    val (kernNameSort, treeSort) = firepile.Compose.compileToTreeName({
        (srcKey: Array[UInt], srcVal: Array[UInt], dstKey: Array[UInt], dstVal: Array[UInt]) => bitonicSortSort(srcKey, srcVal, dstKey, dstVal)}, 4)
    
    val (kernNameMerge, treeMerge) = firepile.Compose.compileToTreeName({
        (srcKey: Array[UInt], srcVal: Array[UInt], arrayLen: Array[UInt], size: Array[UInt], stride: Array[UInt], sortDir: Array[UInt], dstKey: Array[UInt], dstVal: Array[UInt]) =>
          bitonicSortMerge(srcKey, srcVal, arrayLen, size, stride, sortDir, dstKey, dstVal)
        }, 8)

    for (t <- treeSort.reverse)
      kernStrSort.append(t.toCL)

    for (t <- treeMerge.reverse)
      kernStrMerge.append(t.toCL)

    val kernBinSort = dev.buildProgramSrc(kernNameSort, kernStrSort.toString)

    val kernBinMerge = dev.buildProgramSrc(kernNameMerge, kernStrMerge.toString)
    
    val bsSort: (CLByteBuffer, CLByteBuffer, CLByteBuffer, CLByteBuffer) => Unit = {
      new Function4[CLByteBuffer, CLByteBuffer, CLByteBuffer, CLByteBuffer, Unit] {
        def apply(srcKey: CLByteBuffer, srcVal: CLByteBuffer, dstKey: CLByteBuffer, dstVal: CLByteBuffer): Unit = {
          kernBinSort.setArg(0, srcKey) 
          kernBinSort.setArg(1, arrayLen)
          kernBinSort.setArg(2, srcVal)
          kernBinSort.setArg(3, arrayLen)
          kernBinSort.setArg(4, dstKey)
          kernBinSort.setArg(5, arrayLen)
          kernBinSort.setArg(6, dstVal)
          kernBinSort.setArg(7, arrayLen)

          kernBinSort.setLocalArg(8, dev.memConfig.localMemSize * sizeA1)
          kernBinSort.setArg(9, dev.memConfig.localMemSize)
          kernBinSort.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))

        }
      }

    }
    
    val bsMerge: (CLByteBuffer, CLByteBuffer, CLByteBuffer, CLByteBuffer, CLByteBuffer, CLByteBuffer, CLByteBuffer, CLByteBuffer) => Unit = {
      new Function8[CLByteBuffer,CLByteBuffer,CLByteBuffer,CLByteBuffer,CLByteBuffer,CLByteBuffer,CLByteBuffer,CLByteBuffer,Unit] {
        def apply(srcKey: CLByteBuffer, srcVal: CLByteBuffer, arrayLength: CLByteBuffer, size: CLByteBuffer, stride: CLByteBuffer, sortDir: CLByteBuffer, dstKey: CLByteBuffer, dstVal: CLByteBuffer): Unit = {
          kernBinSort.setArg(0, srcKey) 
          kernBinSort.setArg(1, arrayLen)
          kernBinSort.setArg(2, srcVal)
          kernBinSort.setArg(3, arrayLen)
          kernBinSort.setArg(4, arrayLength)
          kernBinSort.setArg(5, arrayLength.getElementCount / sizeA1)
          kernBinSort.setArg(6, size)
          kernBinSort.setArg(7, size.getElementCount / sizeA1)
          kernBinSort.setArg(8, stride)
          kernBinSort.setArg(9, stride.getElementCount / sizeA1)
          kernBinSort.setArg(10, sortDir)
          kernBinSort.setArg(11, sortDir.getElementCount / sizeA1)
          kernBinSort.setArg(12, dstKey)
          kernBinSort.setArg(13, arrayLen)
          kernBinSort.setArg(14, dstVal)
          kernBinSort.setArg(15, arrayLen)
          
          kernBinSort.setLocalArg(16, dev.memConfig.localMemSize * sizeA1)
          kernBinSort.setArg(17, dev.memConfig.localMemSize)
          kernBinSort.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))

        }
      }
        
    }

    val bufA1 = transA1.toBuffer(a1).head
    val bufA2 = transA2.toBuffer(a2).head
         
    val bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
    val bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)

    val arrayLenU = Array[UInt](arrayLen.toUInt)
    val sizeU = Array[UInt](0.toUInt)
    val strideU = Array[UInt](0.toUInt)
    val sortDirU = Array[UInt](1.toUInt)

    // not sure about being able to reuse a trans here
    val bufArrayLen = transA1.toBuffer(arrayLenU.asInstanceOf[A1]).head
    val bufSortDir = transA1.toBuffer(sortDirU.asInstanceOf[A1]).head
    val bufArrayLenCL = dev.context.createByteBuffer(CLMem.Usage.Input, bufArrayLen, true)
    val bufSortDirCL = dev.context.createByteBuffer(CLMem.Usage.Input, bufSortDir, true)

    dev.setWorkSizes(arrayLen / 2, LOCAL_SIZE_LIMIT / 2)
    dev.setLocalMemSize(LOCAL_SIZE_LIMIT)

    bsSort(bufA1CLBuf, bufA2CLBuf, bufKeyOut, bufValOut)

    dev.setWorkSizes(arrayLen / 2, 0)
    dev.setLocalMemSize(1)     // can't be zero
    
    sizeU(0) = (2 * LOCAL_SIZE_LIMIT).toUInt
    while (sizeU(0) <= arrayLen.toUInt) {
      val bufSizeU = transA1.toBuffer(sizeU.asInstanceOf[A1]).head
      val bufSizeUCL = dev.context.createByteBuffer(CLMem.Usage.Input, bufSizeU, true)
      
      strideU(0) = (sizeU(0) / 2).toUInt
      while (strideU(0) > 0.toUInt) {
        val bufStrideU = transA1.toBuffer(strideU.asInstanceOf[A1]).head
        val bufStrideUCL = dev.context.createByteBuffer(CLMem.Usage.Input, bufStrideU, true)
        bsMerge(bufKeyOut, bufValOut, bufArrayLenCL, bufSizeUCL, bufStrideUCL, bufSortDirCL, bufKeyOut, bufValOut)
        
        strideU(0) = strideU(0) >> 1
      }
      sizeU(0) = sizeU(0) << 1
    }

    val bufOutKey = allocDirectBuffer(arrayLen * sizeA1)
    val bufOutVal = allocDirectBuffer(arrayLen * sizeA1)
    bufKeyOut.read(dev.queue, bufOutKey, true)
    bufValOut.read(dev.queue, bufOutVal, true)

    dev.queue.finish

    bufOutKey.rewind
    bufOutVal.rewind

    Array.copy(transA1.fromBuffer(List(bufOutKey)).asInstanceOf[AnyRef], 0, dstKeyOut.asInstanceOf[AnyRef], 0, arrayLen)
    Array.copy(transA1.fromBuffer(List(bufOutVal)).asInstanceOf[AnyRef], 0, dstValOut.asInstanceOf[AnyRef], 0, arrayLen)

   (dstKeyOut, dstValOut)
 }


/*    
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
*/

  def bitonicSortSort(srcKey: Array[UInt], srcVal: Array[UInt], dstKey: Array[UInt], dstVal: Array[UInt]) = (id: Id1, ldata: Array[UInt]) => {
    val LOCAL_SIZE_LIMIT = 1024.toUInt
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
  
    
    dstKey(startPos.toInt) = ldata(id.local * 2)
    dstVal(startPos.toInt) = ldata(id.local * 2 + 1)
    dstKey(startPos + (LOCAL_SIZE_LIMIT / 2)) = ldata((id.local + (LOCAL_SIZE_LIMIT / 2)) * 2)
    dstVal(startPos + (LOCAL_SIZE_LIMIT / 2)) = ldata((id.local + (LOCAL_SIZE_LIMIT / 2)) * 2 + 1)
 
  }


  def bitonicSortMerge(srcKey: Array[UInt], srcVal: Array[UInt], arrayLenA: Array[UInt], sizeA: Array[UInt], strideA: Array[UInt], sortDirA: Array[UInt], dstKey: Array[UInt], dstVal: Array[UInt]) = (id: Id1, ldata: Array[UInt]) => {
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

    dstKey(pos.toInt) = keyA
    dstVal(pos.toInt) = valA
    dstKey(pos + stride.toInt) = keyB
    dstVal(pos + stride.toInt) = valB
  }

}

