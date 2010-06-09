package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestMR1 {
  def main(args: Array[String]) = {
    implicit val gpu: Device = firepile.gpu

    val dataSize = if (args.length > 0) args(0).toInt else 1000

    val b = BBArray.tabulate(dataSize)(_.toFloat)

    {
      val c: Float = time {
        import firepile.Compose._
        val result = b.mapk(f2Mapper1((x:Float)=>x*2)).reduce(f2Reducer((x:Float,y:Float)=>x+y)).start
        result.force
      }
      println("c = " + c)
      val correct = b.map((x:Float)=>x*2).reduceLeft((x:Float,y:Float)=>x+y)
      println("correct sum = " + correct)
      // assert(c == correct)
    }
    
    {
      val c: BBArray[Float] = time {
        import firepile.Compose._
        val result = b.mapk(f2Mapper1((x:Float)=>x*2)).start
        result.force
      }
      println("c = " + c)
    }
    
    {
      val c: BBArray[Float] = time {
        import firepile.Compose._
        val result = b.mapk(f2Mapper1((x:Float)=>x*2)).reduceBlock(f2Reducer((x:Float,y:Float)=>x+y)).start
        result.force
      }
      println("c = " + c)
    }
  }
}
