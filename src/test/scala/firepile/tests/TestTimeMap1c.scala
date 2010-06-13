package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestTimeMap1c {
  val floatX2 = (a:Float) => a * 2.0f

  def main(args: Array[String]) = {
    java.lang.System.runFinalizersOnExit(true)

    implicit val gpu: Device = firepile.gpu

    println("global mem size = " + gpu.globalMemSize)
    println("local mem size = " + gpu.localMemSize)
    println("freq = " + gpu.maxClockFrequency)
    println("max cu = " + gpu.maxComputeUnits)
    println("max group size = " + gpu.maxWorkGroupSize)
    println("max item sizes = " + gpu.maxWorkItemSizes)

    val dataSize = if (args.length > 0) args(0).toInt else 1000
    val n = if (args.length > 1) args(1).toInt else 128

    println("size = " + dataSize)

    val a = Array.tabulate(dataSize)(_.toFloat)

    {
      println("1 floatX2 compile");
      val k = time {
          val k: BBArrayMapKernel1[Float,Float] = floatX2
          k
      }

      val bx: BBArray[Float] = a
      val b: BBArray[Float] = bx.directCopy

      println(n + " bbarray map x2");
      time {
        for (i <- 0 until n) {
          System.gc
          val result = b.mapKernel(k).start
          result.force
        }
      }
    }
  }
}
