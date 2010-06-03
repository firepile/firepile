package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestMap2b {
  val aSinB = (a:Float,b:Float) => a * math.sin(b).toFloat + 1.0f

  def main(args: Array[String]) = {
    implicit val gpu: Device = firepile.gpu

    val dataSize = if (args.length > 0) args(0).toInt else 1000

    val b1 = BBArray.tabulate(dataSize)(_.toFloat)
    val b2 = BBArray.tabulate(dataSize)(_.toFloat).reverse

    println("cl bbarray a*sin(b)+1");
    {
      val c: BBArray[Float] = time {
        val result = mapKernel(aSinB)(b1, b2).start
        result.force
      }
      println("c = " + c)
      assert(c.length == b1.length)
      for (i <- 0 until b1.length) {
        val x = aSinB(b1(i), b2(i))
        assert((x - c(i)).abs < 1e-4)
      }
    }
  }
}
