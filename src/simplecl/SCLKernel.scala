package simplecl

import com.nativelibs4java.opencl._
import scala.collection.mutable.ArrayBuffer
import java.nio.Buffer
// import simplecl.util.Buffer

class SCLKernel(clk: CLKernel) {
  val _CLKernel: CLKernel = clk

   // ONLY WORKS FOR CLFLOATBUFFER
//  def setArgs(args: SCLFloatBuffer*): Unit = {
//    val underlyingBufs = new ArrayBuffer[CLFloatBuffer]
//
//    args.foreach(b => underlyingBufs += b._underlyingBuffer)
//    _CLKernel.setArgs(underlyingBufs.toArray:_*)
//  }
  
  
  def setArgs(args: SCLBuffer[_]*): Unit = {
    val underlyingBufs = new ArrayBuffer[CLBuffer[_]]

    args.foreach(b => underlyingBufs += b._CLBuffer)
    _CLKernel.setArgs(underlyingBufs.toArray:_*)
  }


//  def setArgs[B <: java.nio.Buffer](args: SCLBuffer[B]*): Unit = {
//   val underlyingBufs = new ArrayBuffer[CLBuffer[B]]

//    args.foreach(b => underlyingBufs += b._CLBuffer)
//    _CLKernel.setArgs(underlyingBufs.toArray:_*)
//  }


  def enqueueNDRange(queue: SCLQueue, globalWorkSizes: Array[Int], localWorkSizes: Array[Int], eventsToWaitFor: CLEvent*) = {
  _CLKernel.enqueueNDRange(queue._CLQueue, globalWorkSizes, localWorkSizes, eventsToWaitFor:_*)
  }
}


