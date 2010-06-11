package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._

import java.nio.ByteBuffer
import java.nio.ByteOrder

import scala.reflect.Manifest

import com.nativelibs4java.opencl.CLByteBuffer
import com.nativelibs4java.opencl.CLEvent
import com.nativelibs4java.opencl.CLKernel
import com.nativelibs4java.opencl.CLKernel.LocalSize

import firepile._

class InstantiatedBufKernel(dev: Device, code: CLKernel, val dist: Dist, val effect: Effect, buffers: ByteBuffer*) extends Future[List[ByteBuffer]] {
  def printBuffer(b: ByteBuffer) = {
    val fb = b.asFloatBuffer.duplicate
    var sep = "buffer "
    while (fb.hasRemaining) {
      print(sep)
      print(fb.get)
      sep = " "
    }
    println()
  }

  def run = {
    for (b <- buffers)
      printBuffer(b)

    buffers.foreach(buffer => {
        println("sending " + buffer.limit + " bytes")
    })

    val d = dist
    val e = effect
    val memIn = buffers.map(buffer => dev.global.allocForRead(buffer.limit))
    val lenIn = buffers.map(buffer => dev.global.allocForRead(fixedSizeMarshal[Int].size))
    val memOut = e.outputSizes.map(n => dev.global.allocForWrite(n))
    val lenOut = e.outputSizes.map(n => dev.global.allocForRead(fixedSizeMarshal[Int].size))

    println("reading back " + e.outputSizes + " bytes")

    val writeEvents = (buffers zip memIn).map{ case (buffer,mem) => mem.write(dev.queue, buffer, false) }
    val writeLenEvents = (buffers zip lenIn).map{ case (buffer,mem) => mem.write(dev.queue, {
        val b = ByteBuffer.allocate(fixedSizeMarshal[Int].size).order(ByteOrder.nativeOrder)
        b.putInt(buffer.limit)
        b
      }, false) }
    val writeOutLenEvents = (e.outputSizes zip lenOut).map{ case (len,mem) => mem.write(dev.queue, {
        val b = ByteBuffer.allocate(fixedSizeMarshal[Int].size).order(ByteOrder.nativeOrder)
        b.putInt(len)
        b
      }, false) }

    val localMem: List[LocalSize] = e.localBufferSizes.map(size => dev.local.allocForReadWrite(size))
    val localLen: List[CLByteBuffer] = e.localBufferSizes.map(size => dev.global.allocForRead(fixedSizeMarshal[Int].size))

    val writeLocalLenEvents = (e.localBufferSizes zip lenIn).map{ case (len,mem) => mem.write(dev.queue, {
        val b = ByteBuffer.allocate(fixedSizeMarshal[Int].size).order(ByteOrder.nativeOrder)
        b.putInt(len)
        b
      }, false) }

    val args0: List[CLByteBuffer] = (memIn zip lenIn).flatMap[CLByteBuffer, Seq[CLByteBuffer]]{ case (mem,len) => mem::len::Nil }.toList
    val args1: List[CLByteBuffer] = (memOut zip lenOut).flatMap[CLByteBuffer, Seq[CLByteBuffer]]{ case (mem,len) => mem::len::Nil }.toList
    val args3: List[Object] = (localMem zip localLen).flatMap[Object, Seq[Object]]{ case (mem,len) => mem::len::Nil }.toList
    val args = args0 ::: args1 ::: args3
    println(args)
    code.setArgs(args:_*)

    println("total items = " + d.totalNumberOfItems)
    println("items/group = " + d.numberOfItemsPerGroup)

    val runEvent = code.enqueueNDRange(dev.queue, Array(d.totalNumberOfItems), Array(d.numberOfItemsPerGroup), (writeEvents.toList ::: writeLenEvents.toList ::: writeLocalLenEvents.toList ::: writeOutLenEvents):_*)

    this.runEvent = runEvent
    this.memOut = memOut
  }

  var runEvent: CLEvent = null
  var memOut: List[CLByteBuffer] = null

  def finish = {
    val bufOuts = (effect.outputSizes zip memOut).map {
      case (len,mem) => {
        val bufOut = ByteBuffer.allocateDirect(len).order(ByteOrder.nativeOrder)

        // val readEvent = memOut.read(dev.queue, bufOut, false, runEvent)
        println("runEvent about to be done: " + runEvent)
        runEvent.waitFor
        val readEvent = mem.read(dev.queue, bufOut, false)
        println("readEvent about to be done: " + readEvent)
        readEvent.waitFor
        println("queue done")
        bufOut.rewind
        bufOut
      }
    }
    dev.queue.finish
    bufOuts
  }
}
