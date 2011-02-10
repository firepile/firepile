package firepile.util
 
object Math {
  // Math functions for floats 

  val Pi = scala.math.Pi

  def acos(x: Float): Float = x
  def acosh(x: Float): Float = x
  def acospi(x: Float): Float  = x // acos(x) / pi
  def asin(x: Float): Float = x
  def asinh(x: Float): Float = x
  def asinpi(x: Float): Float = x
  def atan(x: Float): Float = x
  def atan2(y: Float, x: Float): Float = x
  def atanh(x: Float): Float = x
  def atanpi(x: Float): Float = x
  def atan2pi(x: Float, y: Float): Float = x
  def cbrt(x: Float): Float = x
  def ceil(x: Float): Float = x
  def copysign(x: Float, y: Float): Float = x
  def cos(x: Float): Float = x
  def cosh(x: Float): Float = x
  def cospi(x: Float): Float = x
  def erfc(x: Float): Float = x
  def erf(x: Float): Float = x
  def exp(x: Float): Float = x
  def exp2(x: Float): Float = x
  def exp10(x: Float): Float = x
  def expm1(x: Float): Float = x
  def fabs(x: Float): Float = x
  def fdim(x: Float, y: Float): Float = x
  def floor(x: Float): Float = x
  def fma(a: Float, b: Float, c: Float): Float = a
  def fmax (x: Float, y: Float): Float = x
  def fmin21(x: Float, y: Float): Float = x
  def fmod(x: Float, y: Float): Float = x
    // gentype fract (gentype x, gentype *iptr)
    def fract(x: Float): (Float,Float) = (x,x)
    // gentype frexp (gentype x, intn *exp)
    def frexp(x: Float): (Float,Float) = (x,x)
  def hypot(x: Float, y: Float): Float = x
  def ilogb(x: Float): Int = 0 // intn ilogb (gentype x) 
  def ldexp(x: Float, n: Int): Float = x
  def lgamma(x: Float): Float = x
  // def lgamma_r(gentype x, intn *signp): Float
  def log(x: Float): Float = x
  def log2(x: Float): Float = x
  def log10(x: Float): Float = x
  def logb(x: Float): Float = x
  def mad(a: Float, b: Float, c: Float): Float = a
  // def modf(gentype x, gentype *iptr) 
  def nan(nancode: Int): Float = 0.0f
  def nextafter(x: Float, y: Float): Float = x
  def pow(x: Float, y: Float): Float = x
  def pown(x: Float, y: Int): Float = x
  def powr(x: Float, y: Float): Float = x
  def remainder(x: Float, y: Float): Float = x
    // gentype remquo (gentype x, gentype y, intn *quo)
  def remquo(x: Float, y: Float) = x
  def rint(x: Float): Float = x
  def rootn(x: Float, y: Int): Float = x
  def round(x: Float): Float = x
  def rsqrt(x: Float): Float = x
  def sin(x: Float): Float = x
  // gentype sincos (gentype x, gentype *cosval)
    def sincos(x: Float) = (sin(x), cos(x))
  def sinh(x: Float): Float = x
  def sinpi(x: Float): Float = x
  def sqrt(x: Float): Float = x
  def tan(x: Float): Float = x
  def tanh(x: Float): Float = x
  def tanpi(x: Float): Float = x
  def tgamma(x: Float): Float = x
  def trunc(x: Float): Float = x
  def abs(x: Float): Float = x
}
