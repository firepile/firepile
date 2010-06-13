package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestTimeMap2a {
  val floatX2 = (a:Int) => {
	var i = 0
	var j = 0
        while (j < 32) {
           i += (a >>> j) & 1
	   j += 1
	}
	i
  }

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

    val a = Array.tabulate(dataSize)(_.toInt)

    {
      println("1 floatX2 compile");
      val k = time {
          val k: BBArrayMapKernel1[Int,Int] = floatX2
          k
      }

      println(n + " array map x2");
      time {
        for (i <- 0 until n) {
          System.gc
          val result = a.mapKernel(k).start
          result.force
        }
      }
    }
  }
}
