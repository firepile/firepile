package simplecl

import com.nativelibs4java.opencl._
import scala.collection.mutable.ArrayBuffer

class SCLMem(clm: CLMem) {
  val _CLMem: CLMem = clm

  def aquireGLObject(queue: SCLQueue, eventsToWaitFor: SCLEvent*): SCLEvent = {
    val a = new ArrayBuffer[CLEvent]
    eventsToWaitFor.foreach(e => a += e._CLEvent)
    new SCLEvent(_CLMem.acquireGLObject(queue._CLQueue, a.toArray:_*))
  }

  override def equals(obj: Any): Boolean = {
    _CLMem.equals(obj)
  }

  def byteCount: Long = {
    _CLMem.getByteCount
  }

  def context: SCLContext = {
    new SCLContext(_CLMem.getContext)
  }

  def GLObjectInfo: SCLMemUtil.GLObjectInfo = {
    _CLMem.getGLObjectInfo
  }

  override def hashCode: Int = {
    _CLMem.hashCode
  }

  def release: Unit = {
    _CLMem.release
  }

  def releaseGLObject(queue: SCLQueue, eventsToWaitFor: SCLEvent*): SCLEvent = {
    val a = new ArrayBuffer[CLEvent]
    eventsToWaitFor.foreach(e => a += e._CLEvent)
    new SCLEvent(_CLMem.releaseGLObject(queue._CLQueue, a.toArray:_*))
  }


}

object SCLMemUsage {
  val Input = CLMem.Usage.Input
  val Output = CLMem.Usage.Output
  val InputOutput = CLMem.Usage.InputOutput
}

object SCLMemUtil {
  type GLObjectInfo = CLMem.GLObjectInfo
}

