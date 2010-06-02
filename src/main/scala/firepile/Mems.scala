package firepile

import firepile.util.BufferBackedArray._

import java.nio.ByteBuffer
import java.nio.ByteOrder

import com.nativelibs4java.opencl.CLByteBuffer
import com.nativelibs4java.opencl.CLKernel.LocalSize
import com.nativelibs4java.opencl.CLMem

object Mems {
  trait Mem[Buf] {
    protected def alloc(usage: CLMem.Usage, n: Int): Buf
    def allocForReadWrite(n: Int) = alloc(CLMem.Usage.InputOutput, n)
    def allocForRead(n: Int) = alloc(CLMem.Usage.Input, n)
    def allocForWrite(n: Int) = alloc(CLMem.Usage.Output, n)
  }

  class GlobalMem(dev: Device) extends Mem[CLByteBuffer] {
    protected def alloc(usage: CLMem.Usage, size: Int): CLByteBuffer = usage match {
      case CLMem.Usage.Input       => dev.context.createByteBuffer(usage, ByteBuffer.allocate(size).order(ByteOrder.nativeOrder), true)
      case CLMem.Usage.InputOutput => dev.context.createByteBuffer(usage, ByteBuffer.allocate(size).order(ByteOrder.nativeOrder), true)
      case CLMem.Usage.Output      => dev.context.createByteBuffer(usage, ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder), true)
    }
  }

  class LocalMem(dev: Device) extends Mem[LocalSize] with Barrier {
    protected def alloc(usage: CLMem.Usage, size: Int): LocalSize = new LocalSize(size)
  }
}
