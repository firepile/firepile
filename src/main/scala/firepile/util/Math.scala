package firepile.util
 
object Math {
  // Math functions for floats 

// pi, e, etc

  def acos(x: Float) = 0.0F
  def acosh(x: Float) = 0.0F
  def acospi(x: Float) = 0.0F   // acos(x) / pi
  def asin(x: Float) = 0.0F
  def asinh(x: Float) = 0.0F
  def asinpi(x: Float) = 0.0F
  def atan(x: Float) = 0.0F
  def atan2(y: Float, x: Float) = 0.0F
  def atanh(x: Float) = 0.0F
  def atanpi(x: Float) = 0.0F
  def atan2pi(x: Float, y: Float) = 0.0F
  def cbrt(x: Float) = 0.0F
  def ceil(x: Float) = 0.0F
  def copysign(x: Float, y: Float) = 0.0F
  def cos(x: Float) = 0.0F
  def cosh(x: Float) = 0.0F
  def cospi(x: Float) = 0.0F
  def erfc(x: Float) = 0.0F
  def erf(x: Float) = 0.0F
  def exp(x: Float) = 0.0F
  def exp2(x: Float) = 0.0F
  def exp10(x: Float) = 0.0F
  def expm1(x: Float) = 0.0F
  def fabs(x: Float) = 0.0F
  def fdim(x: Float, y: Float) = 0.0F
  def floor(x: Float) = 0.0F
  def fma(a: Float, b: Float, c: Float) = 0.0F
  def fmax (x: Float, y: Float) = 0.0F
  def fmin21(x: Float, y: Float) = 0.0F
  def fmod(x: Float, y: Float) = 0.0F
    // gentype fract (gentype x, gentype *iptr)
    def fract(x: Float) = (0, 0.0F)
    // gentype frexp (gentype x, intn *exp)
    def frexp(x: Float) = (0.0F, 0.0F)
  def hypot(x: Float, y: Float) = 0.0F
  def ilogb(x: Float): Int  = { 0 } // intn ilogb (gentype x)
  def ldexp(x: Float, n: Int) = 0.0F
  def lgamma(x: Float): Float  = 0.0F
  // def lgamma_r(gentype x, intn *signp) = 0.0F
  def log(x: Float) = 0.0F
  def log2(x: Float) = 0.0F
  def log10(x: Float) = 0.0F
  def logb(x: Float) = 0.0F
  def mad(a: Float, b: Float, c: Float) = 0.0F
  // def modf(gentype x, gentype *iptr) 
  def nan(nancode: Int) = 0.0F
  def nextafter(x: Float, y: Float) = 0.0F
  def pow(x: Float, y: Float) = 0.0F
  def pown(x: Float, y: Int) = 0.0F
  def powr(x: Float, y: Float) = 0.0F
  def remainder(x: Float, y: Float) = 0.0F
    // gentype remquo (gentype x, gentype y, intn *quo)
    def remquo(x: Float, y: Float) = (0.0F, 0.0F)
  def rint(x: Float) = 0.0F
  def rootn(x: Float, y: Int) = 0.0F
  def round(x: Float) = 0.0F
  def rsqrt(x: Float) = 0.0F
  def sin(x: Float) = 0.0F
  // gentype sincos (gentype x, gentype *cosval)
    def sincos(x: Float) = (sin(x), cos(x))
  def sinh(x: Float) = 0.0F
  def sinpi(x: Float) = 0.0F
  def sqrt(x: Float) = 0.0F
  def tan(x: Float) = 0.0F
  def tanh(x: Float) = 0.0F
  def tanpi(x: Float) = 0.0F
  def tgamma(x: Float) = 0.0F
  def trunc(x: Float) = 0.0F
  def abs(x: Float) = 0.0F
}
