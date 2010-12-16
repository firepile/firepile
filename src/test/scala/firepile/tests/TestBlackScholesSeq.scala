package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._
import scala.math.sqrt
import scala.math.log
import scala.math.exp
// import firepile.util.Math.{sqrt,log,exp}

object TestBlackScholesSeq {
  def main(args: Array[String]) = {
    val optionCount = if (args.length > 0) args(0).toInt else 4000000
    val n = if (args.length > 1) args(1).toInt else 10

    val rand = new scala.util.Random(2009)

    def randFloat(min: Float, max: Float) = {
      val r = rand.nextDouble
      (min + r * (max - min)).toFloat
    }

    println("SeqBS x " + n + " " + optionCount)


    // val h_Call    = BBArray.tabulate[Float](optionCount)(i => -1.0f)
    // val h_Put     = BBArray.tabulate[Float](optionCount)(i => -1.0f)
    val h_S       = BBArray.tabulate[Float](optionCount)(i => randFloat(5.0f, 30.0f))
    val h_X       = BBArray.tabulate[Float](optionCount)(i => randFloat(1.0f, 100.0f))
    val h_T       = BBArray.tabulate[Float](optionCount)(i => randFloat(0.25f, 10.0f))

    def BlackScholesK(S: Float, X: Float, T: Float): (Float,Float) = {
          val                    R = 0.02f
          val                    V = 0.30f
          var a = 0.0f
          var b = 0.0f
          var i = 0
          while (i < 100) {
            val p = BlackScholesBody(S, X, T, R, V)
            a += p._1
            b += p._2
            i += 1
          }
          (a,b)
    }

      val result = time ({
        for (i <- 0 until n) {
          for (j <- 0 until optionCount) {
            BlackScholesK( h_S(j), h_X(j), h_T(j))
          }
        }
      }, "TestBlackShcoesSeq")

      // val (h_Call, h_Put) = result.unzip

      // println("call " + h_Call)
      // println("put " + h_Put)
      println("done")
  }

///////////////////////////////////////////////////////////////////////////////
// Rational approximation of cumulative normal distribution function
///////////////////////////////////////////////////////////////////////////////
def CND(d: Float): Float = {
    val               A1 = 0.31938153f
    val               A2 = -0.356563782f
    val               A3 = 1.781477937f
    val               A4 = -1.821255978f
    val               A5 = 1.330274429f
    val         RSQRT2PI = 0.39894228040143267793994605993438f

    val K = 1.0f / (1.0f + 0.2316419f * d.abs)

    val cnd = RSQRT2PI * exp(- 0.5f * d * d).toFloat * (K * (A1 + K * (A2 + K * (A3 + K * (A4 + K * A5)))))

    if(d > 0)
        1.0f - cnd
    else
        cnd
}


///////////////////////////////////////////////////////////////////////////////
// Black-Scholes formula for both call and put
///////////////////////////////////////////////////////////////////////////////
def BlackScholesBody(
    S: Float, //Current stock price
    X: Float, //Option strike price
    T: Float, //Option years
    R: Float, //Riskless rate of return
    V: Float  //Stock volatility
)  = {
    val   sqrtT: Float = sqrt(T).toFloat
    val      d1: Float = (log(S / X).toFloat + (R + 0.5f * V * V) * T) / (V * sqrtT)
    val      d2: Float = d1 - V * sqrtT
    val   CNDD1: Float = CND(d1)
    val   CNDD2: Float = CND(d2)

    //Calculate Call and Put simultaneously
    val   expRT: Float = exp(- R * T).toFloat

                          //Call option price
    val call: Float = (S * CNDD1 - X * expRT * CNDD2)
                         //Put option price
    val put: Float  = (X * expRT * (1.0f - CNDD2) - S * (1.0f - CNDD1))

    (call,put)
}


}
