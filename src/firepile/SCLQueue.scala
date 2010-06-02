package simplecl

import com.nativelibs4java.opencl._

class SCLQueue(clq: CLQueue) {
  val _CLQueue: CLQueue = clq

  def finish = _CLQueue.finish

  def enqueueWaitForEvents(events: CLEvent*): Unit = {
    
    val filtered = events.filter(_ != null)
    if (filtered.length != 0)
      _CLQueue.enqueueWaitForEvents(filtered:_*)
  }
}
