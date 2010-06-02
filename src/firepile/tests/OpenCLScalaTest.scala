package simplecl.tests

import simplecl._
import java.nio.FloatBuffer
import java.nio.ByteOrder
import com.nativelibs4java.util.NIOUtils
import simplecl.util.Buffer


object OpenCLScalaTest {
  def main(args: Array[String])  = {
    val dataSize = 1000
    val platform = SimpleCL.listPlatforms(0)
    val devices = platform.listAllDevices(true)

    val context = platform.createContext(null, devices(0))

    devices.foreach(d => println("Device found: vendor = " + d.vendor + " maxComputeUnits = " + d.maxComputeUnits + " at frequency " + d.maxClockFrequency))

    val src = ("\n" + 
			"__kernel void aSinB(                                   \n" +
			"   __global const float* a,                            \n" +
			"   __global const float* b,                            \n" +
			"   __global float* output)                             \n" +
			"{                                                      \n" +
			"   int i = get_global_id(0);                           \n" +
                        "   output[i] = a[i] * sin(b[i]) + 1;\n" +
			"}                                                      \n")

    val program = context.createProgram(src).build
    val kernel = program.createKernel("aSinB")
    val queue = context.createDefaultQueue()

    // Should not need to use asInstanceOf
    val memIn1 = context.createBuffer[Float](SCLMemUsage.Input, dataSize, true).asInstanceOf[SCLBuffer[FloatBuffer]]
    val memIn2 = context.createBuffer[Float](SCLMemUsage.Input, dataSize, true).asInstanceOf[SCLBuffer[FloatBuffer]]
    val memOut = context.createBuffer[Float](SCLMemUsage.Output, dataSize, true).asInstanceOf[SCLBuffer[FloatBuffer]]

    kernel.setArgs(memIn1, memIn2, memOut)

    // val a = FloatBuffer.allocate(dataSize * 4)
    // val b = FloatBuffer.allocate(dataSize * 4)
    val a = Buffer.make[Float](dataSize)
    val b = Buffer.make[Float](dataSize)

    // Fill buffers with some data
    for(i <- 0 to dataSize-1) {
      a.put(i, i)
      b.put(i, i)
    }


    memIn1.write(queue, a, true)
    memIn2.write(queue, b, true)

    // Asking for execution of the kernel with global size = dataSize, workgroup size = 1
    kernel.enqueueNDRange(queue, Array(dataSize), Array(1))

    // Wait for operations to be performed
    queue.finish

    //val output = NIOUtils.directFloats(dataSize, ByteOrder.nativeOrder)
    val output = Buffer.makeDirect[Float](dataSize)

    memOut.read(queue, output, true)

    // Compute absolute and relative average errors wrt Java impl
    var totalAbsError = 0.0
    var totalRelError = 0.0

    for(i <- 0 to dataSize-1) {
      var exp = i * Math.sin(i) + 1
      var res = output.get(i)
      var d = res - exp
    
      if(exp != 0.0)
        totalRelError += d / exp
      if(d < 0) totalAbsError += -d else totalAbsError += d
    }

    var avgAbsError = totalAbsError / dataSize
    var avgRelError = totalRelError / dataSize

    println("Average absolute error = " + avgAbsError)
    println("Average relative error = " + avgRelError)

  }
}


