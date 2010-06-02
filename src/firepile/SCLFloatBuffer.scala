package simplecl

import com.nativelibs4java.opencl._
import java.nio.FloatBuffer

class SCLFloatBuffer(clfb: CLFloatBuffer) {
  val _CLFloatBuffer: CLFloatBuffer = clfb
  val _underlyingBuffer = clfb

  def write(queue: SCLQueue, buff: FloatBuffer, block: Boolean, eventsToWaitFor: CLEvent*): CLEvent = {
    _CLFloatBuffer.write(queue._CLQueue, buff, block, eventsToWaitFor:_*)

  }

  def read(queue: SCLQueue, buff: FloatBuffer, block: Boolean, eventsToWaitFor: CLEvent*): CLEvent = {
    _CLFloatBuffer.read(queue._CLQueue, buff, block, eventsToWaitFor:_*)
  }

}

