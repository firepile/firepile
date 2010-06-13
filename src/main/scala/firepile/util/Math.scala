package firepile.util
 
object Math {
  // Math functions for floats 

  val Pi = scala.math.Pi

  @native def acos(x: Float): Float
  @native def acosh(x: Float): Float
  @native def acospi(x: Float): Float   // acos(x) / pi
  @native def asin(x: Float): Float
  @native def asinh(x: Float): Float
  @native def asinpi(x: Float): Float
  @native def atan(x: Float): Float
  @native def atan2(y: Float, x: Float): Float
  @native def atanh(x: Float): Float
  @native def atanpi(x: Float): Float
  @native def atan2pi(x: Float, y: Float): Float
  @native def cbrt(x: Float): Float
  @native def ceil(x: Float): Float
  @native def copysign(x: Float, y: Float): Float
  @native def cos(x: Float): Float
  @native def cosh(x: Float): Float
  @native def cospi(x: Float): Float
  @native def erfc(x: Float): Float
  @native def erf(x: Float): Float
  @native def exp(x: Float): Float
  @native def exp2(x: Float): Float
  @native def exp10(x: Float): Float
  @native def expm1(x: Float): Float
  @native def fabs(x: Float): Float
  @native def fdim(x: Float, y: Float): Float
  @native def floor(x: Float): Float
  @native def fma(a: Float, b: Float, c: Float): Float
  @native def fmax (x: Float, y: Float): Float
  @native def fmin21(x: Float, y: Float): Float
  @native def fmod(x: Float, y: Float): Float
    // gentype fract (gentype x, gentype *iptr)
    @native def fract(x: Float): (Float,Float)
    // gentype frexp (gentype x, intn *exp)
    @native def frexp(x: Float): (Float,Float)
  @native def hypot(x: Float, y: Float): Float
  @native def ilogb(x: Float): Int // intn ilogb (gentype x)
  @native def ldexp(x: Float, n: Int): Float
  @native def lgamma(x: Float): Float
  // def lgamma_r(gentype x, intn *signp): Float
  @native def log(x: Float): Float
  @native def log2(x: Float): Float
  @native def log10(x: Float): Float
  @native def logb(x: Float): Float
  @native def mad(a: Float, b: Float, c: Float): Float
  // def modf(gentype x, gentype *iptr) 
  @native def nan(nancode: Int): Float
  @native def nextafter(x: Float, y: Float): Float
  @native def pow(x: Float, y: Float): Float
  @native def pown(x: Float, y: Int): Float
  @native def powr(x: Float, y: Float): Float
  @native def remainder(x: Float, y: Float): Float
    // gentype remquo (gentype x, gentype y, intn *quo)
    @native def remquo(x: Float, y: Float)
  @native def rint(x: Float): Float
  @native def rootn(x: Float, y: Int): Float
  @native def round(x: Float): Float
  @native def rsqrt(x: Float): Float
  @native def sin(x: Float): Float
  // gentype sincos (gentype x, gentype *cosval)
    def sincos(x: Float) = (sin(x), cos(x))
  @native def sinh(x: Float): Float
  @native def sinpi(x: Float): Float
  @native def sqrt(x: Float): Float
  @native def tan(x: Float): Float
  @native def tanh(x: Float): Float
  @native def tanpi(x: Float): Float
  @native def tgamma(x: Float): Float
  @native def trunc(x: Float): Float
  @native def abs(x: Float): Float
}
