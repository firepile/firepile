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

object TestBlackScholes {
  def main(args: Array[String]) = {
    val optionCount = if (args.length > 0) args(0).toInt else 4000000
    val n = if (args.length > 1) args(1).toInt else 10

    implicit val gpu: Device = firepile.gpu

    val rand = new scala.util.Random(2009)

    def randFloat(min: Float, max: Float) = {
      val r = rand.nextDouble
      (min + r * (max - min)).toFloat
    }

    println("BS x " + n + " " + optionCount)


    // val h_Call    = BBArray.tabulate[Float](optionCount)(i => -1.0f)
    // val h_Put     = BBArray.tabulate[Float](optionCount)(i => -1.0f)
    val h_S       = Array.tabulate[Float](optionCount)(i => randFloat(5.0f, 30.0f))
    val h_X       = Array.tabulate[Float](optionCount)(i => randFloat(1.0f, 100.0f))
    val h_T       = Array.tabulate[Float](optionCount)(i => randFloat(0.25f, 10.0f))



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
  }
    
  def BlackScholes(S: Array[Float], X: Array[Float], T: Array[Float]): (Float, Float) = {
    implicit val gpu: Device = firepile.gpu

    val bs: (Array[Float], Array[Float], Array[Float], Array[Float]) => Unit = firepile.Compiler.compile {
      (S: Array[Float], X: Array[Float], T: Array[Float], CP: Array[Float]) =>
        blackScholesK(S, X, T, CP)
      }

    val CPOut = Array[Float](2)
    bs(S, X, T, CPOut)

    (CPOut(0), CPOut(1))
  } 

  def blackScholesK(S: Array[Float], X: Array[Float], T: Array[Float], Out: Array[Float]) = (id: Id1, ldata: Array[Float]) => {
    val                    R = 0.02f
    val                    V = 0.30f
    // waste time
    var i = 0
    var p = 0.f
    var c = 0.f

    var globalS = S(id.global)
    var globalX = X(id.global)
    var globalT = T(id.global)

    while (i < 100) {
      p += BlackScholesBodyP(globalS, globalX, globalT, R, V)
      c += BlackScholesBodyC(globalS, globalX, globalT, R, V)
      i += 1
    }
    
    Out(0) = p
    Out(1) = c
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

    val K = 1.0f / (1.0f + 0.2316419f * fabs(d))

    val cnd = RSQRT2PI * exp(- 0.5f * d * d).toFloat * (K * (A1 + K * (A2 + K * (A3 + K * (A4 + K * A5)))))

    if(d > 0)
        1.0f - cnd
    else
        cnd
}


///////////////////////////////////////////////////////////////////////////////
// Black-Scholes formula for both call and put
///////////////////////////////////////////////////////////////////////////////
def BlackScholesBodyP(
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

                         //Put option price
    val put: Float  = (X * expRT * (1.0f - CNDD2) - S * (1.0f - CNDD1))
    put
}

def BlackScholesBodyC(
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
    call
}

}
