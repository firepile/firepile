package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestDotProduct {
  def main(args: Array[String]) = {
    implicit val gpu: Device = firepile.gpu

    val dataSize = if (args.length > 0) args(0).toInt else 1000

    val b1 = BBArray.tabulate(dataSize)(_.toFloat)
    val b2 = BBArray.tabulate(dataSize)(_.toFloat).reverse

    println("cl bbarray dot product");
    {
      /*
      // Ideal code
      spawn {
        (b1, b2).zipWith(_*_).reduce(_+_)
      }
      */

      val c: Float = time {
        // val result = (b1,b2).zipWithKernel((x:Float,y:Float)=>x*y).reduceKernel(_+_)
        // result.force
        import firepile.Compose._
        val result = spawn { (b1,b2).mapk((x:Float,y:Float)=>x*y).reduce((x:Float,y:Float)=>x+y)(b1,b2) }
        result.force
      }
      println("c = " + c)
      val correct = (b1 zip b2).map((p: (Float,Float)) => p._1 * p._2).reduceLeft(_+_)
      println("correct sum = " + correct)
      assert(c == correct)
    }
  }
}
