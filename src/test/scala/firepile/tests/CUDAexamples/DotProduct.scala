object DotProduct {
  // KERNEL
  def dotProduct(/* global */ a: Array[Float], /* global */ b: Array[Float],
    /* global c: Array[Float], */ numElements: Int)(id: Id, localMem: LocalMem): Float = {

    // bound check (equivalent to the limit on a for loop in serial C code)
    if (id.global >= numElements)
      return

    val iInOffset = id.global << 2

    // what would we do about a global array that is intended to return results?
    
    // c(id.global) = 
    // we will just return my element for now and assume results are combined elsewhere
    a(iInOffset) * b(iInOffset) +
      a(iInOffset + 1) * b(iInOffset + 1) + 
      a(iInOffset + 2) * b(iInOffset + 2) + 
      a(iInOffset + 3) * b(iInOffset + 3)
  }

  // pairwise dot on two arrays:
  // A: Array[Float4]
  // B: Array[Float4]

  // type Float4 = (Float,Float,Float,Float)
  // type Float2 = (Float,Float)
  // or:
  // case class Float4(x: Float, y: Float, z: Float, w: Float) {
  //    def lo = Float2(x,y)
  //    def hi = Float2(z,w)
  //    def even = Float2(x,z)
  //    def odd = Float2(y,w)
  //    def s0 = x
  //    def s1 = y
  //    def s2 = z
  //    def s3 = w
  //    def xyzw = this
  //    def xy = Float2(x,y)
  //    def xz = Float2(x,z)
  //    ...
  // }

  // dot: (Float4,Float4) => Float
  // C: Array[Float] = (A,B).zipWith(dot)

  // def dot(a: Float4, b: Float4) = a.x * b.x + a.y * b.y ...

}
      
