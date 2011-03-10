package firepile

import firepile._
import firepile.util.BufferBackedArray._
import firepile.Marshaling._

import java.nio.ByteBuffer

import scala.reflect.Manifest
import scala.collection.mutable.ArraySeq

import com.nativelibs4java.opencl.CLMem
import com.nativelibs4java.opencl.CLDevice
import com.nativelibs4java.opencl.CLKernel
import com.nativelibs4java.opencl.CLProgram
import com.nativelibs4java.opencl.CLEvent
import com.nativelibs4java.opencl.CLKernel.LocalSize
import com.nativelibs4java.opencl._
import java.util.EnumSet

import scala.math.{ ceil, pow, log }

import Mems._

object Device {
  object Type extends Enumeration {
    type Type = Value
    val GPU, CPU, Accelerator, Default = Value
  }
  object ExecutionCapability extends Enumeration {
    type ExecutionCapability = Value
    val Kernel, NativeKernel = Value
  }
  object QueueProperties extends Enumeration {
    type QueueProperties = Value
    val OutOfOrder, Profiling = Value
  }
  object MemCacheType extends Enumeration {
    type MemCacheType = Value
    val None, ReadOnly, ReadWrite = Value
  }
}

class Device(platform: Platform, cld: CLDevice) extends DeviceLike(platform, cld) {
  var program: CLProgram = null

  case class MemConfig(globalSize: Int, localSize: Int, localMemSize: Int)
  var memConfig: MemConfig = null

  def setWorkSizes(gs: Int, ls: Int) = memConfig = MemConfig(gs, ls, ls)
  def setLocalMemSize(lms: Int) = memConfig = MemConfig(memConfig.globalSize, memConfig.localSize, lms)

  def buildProgramSrc(name: String, src: String): CLKernel = {
    try {
      program = context.createProgram(src).build
    } catch {
      case e => println("Compile error: " + e)
    }

    val kernel = program.createKernel(name)

    kernel
  }

  def maxThreads = cld.getMaxWorkGroupSize

  def defaultPaddedPartition(n: Int): Space = {

    var threads = (if (n < maxThreads * 2) pow(2, ceil(log(n) / log(2))) else maxThreads).toInt
    var blocks = ((n + (threads * 2 - 1)) / (threads * 2)).toInt

    //Space.setSpace(threads,blocks)
    //MemConfig(blocks, threads, threads)
    Kernel(threads, blocks)
    new Space(threads, blocks)
  }


 private[firepile] lazy val queue = context.createDefaultQueue()

  lazy val global = new GlobalMem(this)
  lazy val local = new LocalMem(this)

  // def spawn[B](k: Future[B]) = k.start

}
