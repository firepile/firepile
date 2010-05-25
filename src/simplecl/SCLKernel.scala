package simplecl

import com.nativelibs4java.opencl._
import scala.collection.mutable.ArrayBuffer
import java.nio.Buffer
// import simplecl.util.Buffer

class SCLKernel(clk: CLKernel) {
  val _CLKernel: CLKernel = clk

  def setArgs(args: Object*): Unit = {
    val unwrapped = (args.map {
        case b: SCLBuffer[_] => b._CLBuffer
        case o: Object => o
    })
    _CLKernel.setArgs(unwrapped:_*)
  }

  def enqueueNDRange(queue: SCLQueue, globalWorkSizes: Array[Int], localWorkSizes: Array[Int], eventsToWaitFor: CLEvent*) = {
    _CLKernel.enqueueNDRange(queue._CLQueue, globalWorkSizes, localWorkSizes, eventsToWaitFor:_*)
  }
}


