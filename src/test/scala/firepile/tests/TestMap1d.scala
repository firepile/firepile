package firepile.tests

import firepile.Firepile
import firepile.Firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestMap1d {
  def main(args: Array[String]) = {
    implicit val gpu: Device = Firepile.gpu

    val dataSize = if (args.length > 0) args(0).toInt else 1000

    val a = BBArray.tabulate(dataSize)(_.toFloat - dataSize / 2)

    println("cl array");
    {
      val c = time {
        val result = a.mapKernel((x:Float) => if (x < 0) -1 else if (x > 0) 1 else 0)
        result.force
      }
      assert(a.length == c.length)
      for (i <- 0 until a.length) {
        println(a(i) + " " + c(i))
        val f = (x:Float) => if (x < 0) -1 else if (x > 0) 1 else 0
        val x = f(c(i))
        val y = f(a(i))
        assert(x == y)
      }
    }
  }
}
