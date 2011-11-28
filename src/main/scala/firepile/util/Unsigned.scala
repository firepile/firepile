package firepile.util

object Unsigned {
// implicit def s2u(x: Int) = UInt(x)
// implicit def u2s(x: UInt) = x.rep

implicit def s2r(x: Int) = R(x)
implicit def s2r(x: scala.runtime.RichInt) = RR(x.self.asInstanceOf[Int])

implicit def s2l(x: Long) = L(x)
implicit def s2l(x: scala.runtime.RichLong) = LL(x.self.asInstanceOf[Long])

// todo: add LongRange

case class L(x: Long) {
  def toUInt = UInt(x.toInt)
}

case class LL(x: Long) {
  // def to(y: UInt): Range.Inclusive = x to y.rep
  // def until(y: UInt): Range = x until y.rep
  // def max(that: UInt) = if (x < that) that else x
  // def min(that: UInt) = if (x > that) that else x
}

case class RR(x: Int) {
  def to(y: UInt): Range.Inclusive = x to y.rep
  def until(y: UInt): Range = x until y.rep
  // def max(that: UInt) = if (x < that) that else x
  // def min(that: UInt) = if (x > that) that else x
}

object Ordering {
  trait UIntOrdering extends Ordering[UInt] {
    def compare(x: UInt, y: UInt) = 
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object UInt extends UIntOrdering
}


// additional operations on ints
case class R(x: Int) {
  def toUInt = UInt(x)
  def u = UInt(x)

  def +(y: UInt) = x + y.rep
  def -(y: UInt) = x - y.rep
  def *(y: UInt) = x * y.rep
  def /(y: UInt) = x / y.rep
  def %(y: UInt) = x % y.rep

  import java.lang.Integer

  def bitCount = Integer.bitCount(x)
  def highestOneBit = Integer.highestOneBit(x)
  def lowestOneBit = Integer.lowestOneBit(x)
  def numberOfLeadingZeros = Integer.numberOfLeadingZeros(x)
  def numberOfTrailingZeros = Integer.numberOfTrailingZeros(x)

  def reverse = Integer.reverse(x)
  def reverseBytes = Integer.reverseBytes(x)
  def rotateLeft(dist: Int) = Integer.rotateLeft(x, dist)
  def rotateRight(dist: Int) = Integer.rotateRight(x, dist)
  def signum = Integer.signum(x)

  def <<@(dist: Int) = rotateLeft(dist)
  def >>@(dist: Int) = rotateRight(dist)

  def abs = (x >> 30 | 1) * x
}

case class UInt(rep: Int) {
  private def rot(x: Int) = (x + Int.MinValue)

  def to(y: UInt): Range.Inclusive = rep to y.rep
  def until(y: UInt): Range = rep until y.rep

  def toInt = rep
  def toByte = rep.toByte
  def toShort = rep.toShort
  def toChar = rep.toChar
  def toBoolean = (rep > 0)
  def toLong = (rep & 0xffffffffL)
  def toFloat = (rep & 0xffffffffL).toFloat
  def toDouble = (rep & 0xffffffffL).toDouble

  def bitCount = Integer.bitCount(rep)
  def highestOneBit = Integer.highestOneBit(rep)
  def lowestOneBit = Integer.lowestOneBit(rep)
  def numberOfLeadingZeros = Integer.numberOfLeadingZeros(rep)
  def numberOfTrailingZeros = Integer.numberOfTrailingZeros(rep)
  def reverse = UInt(Integer.reverse(rep))
  def reverseBytes = UInt(Integer.reverseBytes(rep))
  def rotateLeft(dist: Int) = UInt(Integer.rotateLeft(rep, dist))
  def rotateRight(dist: Int) = UInt(Integer.rotateRight(rep, dist))
  def signum = if (rep == 0) 0 else 1

  def abs = this
  def compare(that: Int): Int = {
      if (that < 0) 1 /* this > that */
      else if (rep < 0) 1 /* this > MAXINT >= that */
      else if (rep < that) 1
      else if (rep > that) -1
      else 0
  }
  def compare(that: UInt): Int = {
      if (this < that) 1
      else if (this > that) -1
      else 0
  }
  def max(that: UInt) = if (this < that) that else this
  def min(that: UInt) = if (this > that) that else this

/*
  def to(end: UInt) = {
      var i = this
      while (i <= end) {
          yield i
          i = UInt(i.rep+1)
      }
  }
  def until(end: UInt) = {
      var i = this
      while (i < end) {
          yield i
          i = UInt(i.rep+1)
      }
  }
  */
  
  // Result of operation with a signed int is signed
  def +(x: Int) = rep + x
  def *(x: Int) = rep * x
  def -(x: Int) = rep - x
  def /(x: Int) = rep / x
  def %(x: Int) = rep % x

  def +(x: UInt) = UInt(rep + x.rep)
  def *(x: UInt) = UInt(rep * x.rep)
  def -(x: UInt) = rep - x.rep // signed

  // Algorithm from Hacker's Delight
  def /(x: UInt) = {
    val n = rep
    val d = x.rep
    val t = d >> 31
    val n_ = n & ~t
    val q = ((n_ >>> 1) / d) << 1
    val r = n - q * d
    q + (if (rot(r) >= rot(d)) 1 else 0)
  }

  // Algorithm from Hacker's Delight
  def %(x: UInt) = {
    val n = rep
    val d = x.rep
    val t = d >> 31
    val n_ = n & ~t
    val q = ((n_ >>> 1) / d) << 1
    n - q * d
  }

  // Comparison to UInt is baked in

  // Here, compare to Int
  def ==(x: Int) = rep == x && rep >= 0
  def !=(x: Int) = rep != x || rep < 0

  def <(x: UInt) = rot(rep) < rot(x.rep)
  def >(x: UInt) = rot(rep) > rot(x.rep)
  def <=(x: UInt) = rot(rep) <= rot(x.rep)
  def >=(x: UInt) = rot(rep) >= rot(x.rep)

  override def toString = (rep & 0xffffffffL).toString + "u"

  def &(x : UInt) = UInt(rep & x.rep)
  def +(x : java.lang.String) = this.toString + x
  def <<(x : Int) = UInt(rep << x)
  def <<(x : Long) = UInt(rep << x)
  def >>(x : Long) = UInt(rep >>> x)
  def >>(x : Int) = UInt(rep >>> x)
  def >>>(x : Int) = UInt(rep >>> x)
  def >>>(x : Long) = UInt(rep >>> x)
  def <<(x : UInt) = UInt(rep >>> (x.rep & 0x1f))
  def >>(x : UInt) = UInt(rep << (x.rep & 0x1f))
  def >>>(x : UInt) = UInt(rep >>> (x.rep & 0x1f))
  def ^(x : UInt) = UInt(rep ^ x.rep)
  def unary_+ = this
  def unary_- = -rep
  def unary_~ = UInt(~rep)
  def |(x : UInt) = UInt(rep | x.rep)
}

   def main(args: Array[String]) = {

        for (u <- 0.toUInt to 10.toUInt) {
                println(u)
        }

        sys.exit(0)

        println(1e9.toLong.toUInt)
        println(2e9.toLong.toUInt)
        println(3e9.toLong.toUInt)
        println(4e9.toLong.toUInt)
        println(1e9.toLong.toUInt|(1).toUInt)
        println(2e9.toLong.toUInt|(1).toUInt)
        println(3e9.toLong.toUInt|(1).toUInt)
        println(4e9.toLong.toUInt|(1).toUInt)

        for (s <- List(1, 2, 4, 5, 8, 9, 10, 15, 16, 17, 22, 0x12345678)) {
            val u = s.toUInt
        for (t <- List(1, 2, 4, 5, 8, 9, 10, 15, 16, 17, 22, 0x12345678)) {
            val v = t.toUInt

            println("(" + u + " / " + v + ") = " + (u / v))
            println("(" + u + " * " + v + ") = " + (u * v))
            println("(" + u + " + " + v + ") = " + (u + v))
            println("(" + u + " - " + v + ") = " + (u - v))

            println("(" + u + " / " + t + ") = " + (u / t))
            println("(" + u + " * " + t + ") = " + (u * t))
            println("(" + u + " + " + t + ") = " + (u + t))
            println("(" + u + " - " + t + ") = " + (u - t))

            println("(" + u + " < " + v + ") = " + (u < v))
            println("(" + u + " > " + v + ") = " + (u > v))
            println("(" + u + " <= " + v + ") = " + (u <= v))
            println("(" + u + " >= " + v + ") = " + (u >= v))
            println("(" + u + " == " + v + ") = " + (u == v))
            println("(" + u + " != " + v + ") = " + (u != v))
        }
        }
    }
}
