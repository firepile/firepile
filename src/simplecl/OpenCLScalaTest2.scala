import simplecl._
import java.nio.CharBuffer
import java.nio.ByteBuffer
import com.nativelibs4java.util.NIOUtils
import simplecl.util.Buffer


object OpenCLScalaTest2 {
  def main(args: Array[String])  = {
    val dataSize = 10
    val platform = SimpleCL.listPlatforms(0)
    val devices = platform.listAllDevices(true)

    val context = platform.createContext(null, devices(0))

    devices.foreach(d => println("Device found: vendor = " + d.vendor + " maxComputeUnits = " + d.maxComputeUnits + " at frequency " + d.maxClockFrequency))

    val src = ("\n" + 
			"__kernel void flip(                                                       \n" +
			"   __global const char* a,                                       \n" +
			"   __global char* output)                                        \n" +
			"{                                                                             \n" +
			"   int i = get_global_id(0);                                      \n" +
			"   output[i] = ~a[i];                                              \n" +
			"}                                                                             \n")

    val program = context.createProgram(src).build
    val kernel = program.createKernel("flip")
    val queue = context.createDefaultQueue()

    // Byte throwing exception. Might be a problem with JavaCL
    val memIn = context.createBuffer[Byte](SCLMemUsage.Input, dataSize , true).asInstanceOf[SCLBuffer[ByteBuffer]]
    val memOut = context.createBuffer[Byte](SCLMemUsage.Output, dataSize, true).asInstanceOf[SCLBuffer[ByteBuffer]]

    // This is wrong
    kernel.setArgs[ByteBuffer](memIn, memOut)

    // val a = Buffer.fromSeq[Byte](List(0xaf.toByte, 0xfa.toByte, 0xaf.toByte, 0xfa.toByte, 0xaf.toByte, 0xfa.toByte, 0xaf.toByte, 0xfa.toByte, 0xaf.toByte, 0xfa.toByte))

    val a = Buffer.make[Byte](dataSize)

    for(i <- 0 until dataSize) {
        a.put(0xaa.toByte)
    }

    memIn.write(queue, a.unwrap.asInstanceOf[ByteBuffer], true)

    // Asking for execution of the kernel with global size = dataSize, workgroup size = 1
    kernel.enqueueNDRange(queue, Array(dataSize), Array(1))

    // Wait for operations to be performed
    queue.finish

    val output = Buffer.makeDirect[Byte](dataSize)

    memOut.read(queue, output.unwrap.asInstanceOf[ByteBuffer], true)

    for(i <- 0 until dataSize) {
      println("was " + a.get(i) + " now " + output.get(i))
    }


  }
}


