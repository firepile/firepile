package firepile.tests

object GpuArrayTest {

  import firepile._
  import scala.util.Random
  import firepile.compiler.collections._
   
  def main(args: Array[String]) = run

  def run = {
    val random = new Random(0)
    val randInput = Array.fill(16384)(random.nextFloat)
    //val a = BBArray.tabulate[Float](1000000)(_.toFloat)
    val gpuSum = GPUArray.reduce(randInput)
    //val b = g.map(_*2).reduce(_+_)
  
    val cpuSum = randInput.sum

    println("CPU sum = " + cpuSum + "   GPU sum = " + gpuSum)
  }

 
}
