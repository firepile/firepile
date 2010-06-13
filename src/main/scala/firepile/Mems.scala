package firepile

import firepile.util.BufferBackedArray.allocDirectBuffer
import firepile.util.BufferBackedArray.allocBuffer

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
    def allocForRead(buffer: ByteBuffer) = createByteBuffer(CLMem.Usage.Input, buffer, if (buffer.isDirect) false else true)
    def allocForReadWrite(buffer: ByteBuffer) = createByteBuffer(CLMem.Usage.InputOutput, buffer, if (buffer.isDirect) false else true)
    def allocForWrite(buffer: ByteBuffer) = createByteBuffer(CLMem.Usage.Output, buffer, if (buffer.isDirect) false else true)

    private def createByteBuffer(kind: CLMem.Usage, b: ByteBuffer, copy: Boolean): CLByteBuffer = dev.context.createByteBuffer(kind, b, copy)

    protected def alloc(usage: CLMem.Usage, size: Int): CLByteBuffer = usage match {
      case CLMem.Usage.Input       => createByteBuffer(usage, allocBuffer(size), true)
      case CLMem.Usage.InputOutput => createByteBuffer(usage, allocBuffer(size), true)
      case CLMem.Usage.Output      => createByteBuffer(usage, allocDirectBuffer(size), true)
    }
  }

  class LocalMem(dev: Device) extends Mem[LocalSize] with Barrier {
    protected def alloc(usage: CLMem.Usage, size: Int): LocalSize = new LocalSize(size)
  }
}
