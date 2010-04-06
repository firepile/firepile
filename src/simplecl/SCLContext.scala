package simplecl

import com.nativelibs4java.opencl._
import java.nio.Buffer
import scala.collection.mutable.ArrayBuffer

class SCLContext(clc: CLContext) {
  val _CLContext: CLContext = clc

  def createProgram(srcs: String*): SCLProgram = { 
    new SCLProgram(clc.createProgram(srcs:_*))
  }

  def createProgram(devices: List[SCLDevice], srcs: String*): SCLProgram = {
    val a = new ArrayBuffer[CLDevice]
    devices.foreach(d => a += d._CLDevice)
    new SCLProgram(clc.createProgram(a.toArray, srcs:_*))
  }

  def createDefaultQueue(queueProperties: SCLDeviceUtil.QueueProperties*): SCLQueue = {
    new SCLQueue(_CLContext.createDefaultQueue(queueProperties:_*))
  }

  def createFloatBuffer(kind: CLMem.Usage, count: Long): SCLFloatBuffer = {
    new SCLFloatBuffer(_CLContext.createFloatBuffer(kind, count))
  }

  def createBuffer[B <: Buffer](kind: CLMem.Usage, buffer: B, copy: Boolean): SCLBuffer[B] = {
    new SCLBuffer[B](_CLContext.createBuffer[B](kind, buffer, copy))
  }
}

