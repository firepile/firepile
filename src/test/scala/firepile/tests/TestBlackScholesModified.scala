package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._
import firepile.Marshaling._
// import scala.math.sqrt
// import scala.math.log
// import scala.math.exp
import firepile.util.Math.{sqrt,log,exp,fabs}

object TestBlackScholesModified {
  def main(args: Array[String]) = {
    val optionCount = if (args.length > 0) args(0).toInt * 1000000 else 4000000
    // val n = if (args.length > 1) args(1).toInt else 10

    implicit val gpu: Device = firepile.gpu

    val rand = new scala.util.Random(2009)

    def randFloat(min: Float, max: Float) = {
      val r = rand.nextDouble
      (min + r * (max - min)).toFloat
    }

    println("BS x "  + optionCount)


    val h_S    = BBArray.tabulate[Float](optionCount)(i => randFloat(5.0f, 30.0f))
    val h_X     = BBArray.tabulate[Float](optionCount)(i => randFloat(1.0f, 100.0f))
    val h_T     = BBArray.tabulate[Float](optionCount)(i => randFloat(0.25f, 10.0f))
	 
    //val h_S       = Array.fill[Float](optionCount)(randFloat(5.0f, 30.0f))
    //val h_X       = Array.fill[Float](optionCount)(randFloat(1.0f, 100.0f))
    //val h_T       = Array.fill[Float](optionCount)(randFloat(0.25f, 10.0f))
     


/*
      var t = 0L
      for (i <- 0 until n) {
        val t0 = System.nanoTime
        val t1 = System.nanoTime
        t += t1 - t0
      }
      println("time " + t/1e9)
      */

/*
      time {
        for (i <- 0 until n) {
          val r = k2(h_S,h_X,h_T)
          r.force
        }
      }
*/

      val (h_Call, h_Put) = BlackScholes(h_S, h_X, h_T)

      println("call " + h_Call)
      println("put " + h_Put)
      
      println("done")

      Kernel.printTime
  }
    
  def BlackScholes(S: BBArray[Float], X: BBArray[Float], T: BBArray[Float])(implicit dev: Device): (Float, Float) = {
    dev.setWorkSizes(60 * 1024, 128)

    val space = dev.defaultPaddedPartition(S.length)
    val n = S.length
    

    val CPOut = new BBArray[Float](S.length*2)
    Kernel.output("CPOut")

    space.spawn {
      space.groups.foreach {
        g => {
          g.items.foreach {
            item => {
              val                    R = 0.02f
              val                    V = 0.30f
              val               A1 = 0.31938153f
              val               A2 = -0.356563782f
              val               A3 = 1.781477937f
              val               A4 = -1.821255978f
              val               A5 = 1.330274429f
              val         RSQRT2PI = 0.39894228040143267793994605993438f

              var i = g.id

              while ( i < n) {  // n = S.length
                val   sqrtT: Float = sqrt(T(i)).toFloat
                val      d1: Float = (log(S(i) / X(i)).toFloat + (R + 0.5f * V * V) * T(i)) / (V * sqrtT)
                val      d2: Float = d1 - V * sqrtT
                val   CNDD1: Float = CND(d1)
                val   CNDD2: Float = CND(d2)


                val   expRT: Float = exp(- R * T(i)).toFloat

                                   //Put option price
                val put: Float  = (X(i) * expRT * (1.0f - CNDD2) - S(i) * (1.0f - CNDD1))
                                    //Call option price
                val call: Float = (S(i) * CNDD1 - X(i) * expRT * CNDD2)


                CPOut(i*2) = put
                CPOut(i*2 + 1) = call 
                i += g.size
              }
            }
          }
        }
      }
      (S,X,T,CPOut,n)
    }
      


    // hardcoded globalWorkSize and localWorkSize similar to nvidia example
    // bs.setWorkSizes(...)

    // Puts are stored at even index numbers, calls are stored at odd index numbers
    var put = 0.f
    var call = 0.f

    for (i <- 0 until CPOut.length)
      if (i % 2 == 0) put += CPOut(i).abs
      else call += CPOut(i).abs

    (call, put)
  } 


///////////////////////////////////////////////////////////////////////////////
// Rational approximation of cumulative normal distribution function
///////////////////////////////////////////////////////////////////////////////
@inline def CND(d: Float): Float = {
    val               A1 = 0.31938153f
    val               A2 = -0.356563782f
    val               A3 = 1.781477937f
    val               A4 = -1.821255978f
    val               A5 = 1.330274429f
    val         RSQRT2PI = 0.39894228040143267793994605993438f

    val K = 1.0f / (1.0f + 0.2316419f * fabs(d))

    val cnd = RSQRT2PI * exp(- 0.5f * d * d).toFloat * (K * (A1 + K * (A2 + K * (A3 + K * (A4 + K * A5)))))

    if(d > 0)
        1.0f - cnd
    else
        cnd
}


}
