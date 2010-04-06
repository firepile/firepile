package simplecl

import com.nativelibs4java.opencl._
import scala.collection.mutable.ArrayBuffer

class SCLKernel(clk: CLKernel) {
  val _CLKernel: CLKernel = clk

  // ONLY WORKS FOR CLFLOATBUFFER
  def setArgs(args: SCLFloatBuffer*): Unit = {
    val underlyingBufs = new ArrayBuffer[CLFloatBuffer]

    args.foreach(b => underlyingBufs += b._underlyingBuffer)
    _CLKernel.setArgs(underlyingBufs.toArray:_*)
  }

  def enqueueNDRange(queue: SCLQueue, globalWorkSizes: Array[Int], localWorkSizes: Array[Int], eventsToWaitFor: CLEvent*) = {
  _CLKernel.enqueueNDRange(queue._CLQueue, globalWorkSizes, localWorkSizes, eventsToWaitFor:_*)
  }
}


