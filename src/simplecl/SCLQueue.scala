package simplecl

import com.nativelibs4java.opencl._

class SCLQueue(clq: CLQueue) {
  val _CLQueue: CLQueue = clq

  def finish = _CLQueue.finish
}
