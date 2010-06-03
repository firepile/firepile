package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestReduce1 {
  val sumFloat: (Float,Float) => Float = (_+_)

  def main(args: Array[String]) = {
    implicit val gpu: Device = firepile.gpu

    val dataSize = if (args.length > 0) args(0).toInt else 1000

    val b = BBArray.tabulate(dataSize)(_.toFloat)

    println("cl bbarray sum");
    {
      val c: Float = time {
        val result = b.reduceKernel(sumFloat)
        result.force
      }
      println("c = " + c)
      val correct = b.sum
      println("correct sum = " + correct)
      assert(c == correct)
    }
  }
}
