package simplecl

import com.nativelibs4java.opencl._
import java.nio.{Buffer => Buf,
                ByteBuffer => ByteBuf}
import simplecl.util.Buffer

class SCLBuffer[B <: Buf](clb: CLBuffer[B]) extends SCLMem(clb) {
  val _CLBuffer = clb

  // These get to the underlying CLBuffers as certain types
  def clByteBuffer: CLByteBuffer = _CLBuffer.asCLByteBuffer
  def clCharBuffer: CLCharBuffer = _CLBuffer.asCLCharBuffer
  def clDoubleBuffer: CLDoubleBuffer = _CLBuffer.asCLDoubleBuffer
  def clFloatBuffer: CLFloatBuffer = _CLBuffer.asCLFloatBuffer
  def clIntBuffer: CLIntBuffer = _CLBuffer.asCLIntBuffer
  def clLongBuffer: CLLongBuffer = _CLBuffer.asCLLongBuffer
  def clShortBuffer: CLShortBuffer = _CLBuffer.asCLShortBuffer


  def write(queue: SCLQueue, buff: B, block: Boolean, eventsToWaitFor: CLEvent*): CLEvent = {
    _CLBuffer.write(queue._CLQueue, buff, block, eventsToWaitFor:_*)
  }

  // for new Buffers
  def write[T](queue: SCLQueue, buff: Buffer[T], block: Boolean, eventsToWaitFor: CLEvent*): CLEvent = {
    _CLBuffer.write(queue._CLQueue, buff.unwrap.asInstanceOf[B], block, eventsToWaitFor:_*)
  }

  def write(queue: SCLQueue, offset: Long, length: Long, in: B, block: Boolean, eventsToWaitFor: CLEvent*): CLEvent = {
    _CLBuffer.write(queue._CLQueue, offset, length, in, block, eventsToWaitFor:_*)
  }

  def read(queue: SCLQueue, eventsToWaitFor: CLEvent*): B = {
    _CLBuffer.read(queue._CLQueue, eventsToWaitFor:_*)
  }

  def read(queue: SCLQueue, buff: B, block: Boolean, eventsToWaitFor: CLEvent*): CLEvent = {
    _CLBuffer.read(queue._CLQueue, buff, block, eventsToWaitFor:_*)
  }

  // for new Buffers
  def read[T](queue: SCLQueue, buff: Buffer[T], block: Boolean, eventsToWaitFor: CLEvent*): CLEvent = {
    _CLBuffer.read(queue._CLQueue, buff.unwrap.asInstanceOf[B], block, eventsToWaitFor:_*)
  }

  def read(queue: SCLQueue, offset: Long, length: Long, out: B, block: Boolean, eventsToWaitFor: CLEvent*): CLEvent = {
    _CLBuffer.read(queue._CLQueue, offset, length, out, block, eventsToWaitFor:_*)
  }

  def map(queue: SCLQueue, flags: CLMem.MapFlags, eventsToWaitFor: CLEvent*): B = {
    _CLBuffer.map(queue._CLQueue, flags, eventsToWaitFor:_*)
  }

  def map(queue: SCLQueue, flags: CLMem.MapFlags, offset: Long, length: Long, eventsToWaitFor: CLEvent*): B = {
    _CLBuffer.map(queue._CLQueue, flags, offset, length, eventsToWaitFor:_*)
  }

  // TODO: mapLater

  
  def readBytes(queue: SCLQueue, offset: Long, length: Long, eventsToWaitFor: CLEvent*): ByteBuf = {
    _CLBuffer.readBytes(queue._CLQueue, offset, length, eventsToWaitFor:_*)
  }

  def writeBytes(queue: SCLQueue, offset: Long, length: Long, in: ByteBuf, block: Boolean,
                        eventsToWaitFor: CLEvent*) = {
    _CLBuffer.writeBytes(queue._CLQueue, offset, length, in, block, eventsToWaitFor:_*)
  }

  override def equals(obj: Any): Boolean = _CLBuffer.equals(obj)

  def elementCount: Long = _CLBuffer.getElementCount

  def elementSize: Int = _CLBuffer.getElementSize

  override def hashCode: Int = _CLBuffer.hashCode


}
