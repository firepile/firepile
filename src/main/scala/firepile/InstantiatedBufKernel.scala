package firepile

import firepile.util.BufferBackedArray._

import java.nio.ByteBuffer
import java.nio.ByteOrder

import scala.reflect.Manifest

import com.nativelibs4java.opencl.CLByteBuffer
import com.nativelibs4java.opencl.CLEvent
import com.nativelibs4java.opencl.CLKernel
import com.nativelibs4java.opencl.CLKernel.LocalSize

import Firepile._

class InstantiatedBufKernel(code: CLKernel, val dist: Dist, val effect: Effect, buffers: ByteBuffer*) extends InstantiatedKernel[ByteBuffer] {
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

  def run(dev: Device) = {
    for (b <- buffers)
      printBuffer(b)

    buffers.foreach(buffer => {
        println("sending " + buffer.limit + " bytes")
    })

    val d = dist
    val e = effect
    val memIn = buffers.map(buffer => dev.global.allocForRead(buffer.limit))
    val lenIn = buffers.map(buffer => dev.global.allocForRead(fixedSizeMarshal[Int].size))
    val memOut = dev.global.allocForWrite(e.outputSize)
    val lenOut = dev.global.allocForRead(fixedSizeMarshal[Int].size)

    println("reading back " + e.outputSize + " bytes")

    val writeEvents = (buffers zip memIn).map{ case (buffer,mem) => mem.write(dev.queue, buffer, false) }
    val writeLenEvents = (buffers zip lenIn).map{ case (buffer,mem) => mem.write(dev.queue, {
        val b = ByteBuffer.allocate(fixedSizeMarshal[Int].size).order(ByteOrder.nativeOrder)
        b.putInt(buffer.limit)
        b
      }, false) }

    val writeOutLenEvent = lenOut.write(dev.queue, {
        val b = ByteBuffer.allocate(fixedSizeMarshal[Int].size).order(ByteOrder.nativeOrder)
        b.putInt(e.outputSize)
        b
      }, false)

    val localMem: List[LocalSize] = e.localBufferSizes.map(size => dev.local.allocForReadWrite(size))
    val args0: List[CLByteBuffer] = (memIn zip lenIn).flatMap[CLByteBuffer, Seq[CLByteBuffer]]{ case (mem,len) => mem::len::Nil }.toList
    val args1: List[CLByteBuffer] = memOut :: lenOut :: Nil
    val args = args0 ::: args1 ::: localMem
    println(args)
    code.setArgs(args:_*)

    println("total items = " + d.totalNumberOfItems)
    println("items/group = " + d.numberOfItemsPerGroup)

    val runEvent = code.enqueueNDRange(dev.queue, Array(d.totalNumberOfItems), Array(d.numberOfItemsPerGroup),
              (writeEvents.toList ::: writeLenEvents.toList ::: writeOutLenEvent :: Nil):_*)

    readBackResult(dev, e.outputSize, runEvent, memOut)
  }

  def readBackResult(dev: Device, outputLength: Int, runEvent: CLEvent, memOut: CLByteBuffer) = {
    new Future[ByteBuffer] {
      def force = {
        val bufOut = ByteBuffer.allocateDirect(outputLength).order(ByteOrder.nativeOrder)
        // val readEvent = memOut.read(dev.queue, bufOut, false, runEvent)
        println("runEvent about to be done: " + runEvent)
        runEvent.waitFor
        val readEvent = memOut.read(dev.queue, bufOut, false)
        println("readEvent about to be done: " + readEvent)
        readEvent.waitFor
        dev.queue.finish
        println("queue done")
        bufOut.rewind
        bufOut
      }
    }
  }
}
