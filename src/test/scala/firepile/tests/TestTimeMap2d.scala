package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestTimeMap2d {
  val bits = (a:Int) => {
        var i = 0
        var j = 0
        while (j < 32) {
           i += (a >>> j) & 1
           j += 1
        }
        i
  }


  def main(args: Array[String]) = {
    val dataSize = if (args.length > 0) args(0).toInt else 1000
    val n = if (args.length > 1) args(1).toInt else 128

    println("size = " + dataSize)

    val a = Array.tabulate(dataSize)(_.toInt)

    {
      val bx: BBArray[Int] = a
      val b: BBArray[Int] = bx.directCopy
      println(n + " bbarray sequential map x2");
      time ({
        for (i <- 0 until n) {
          val result = b.map(bits)
          result
        }
      }, "TimeMap2d")
    }
  }
}
