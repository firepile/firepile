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
      b(iInOffset + 3) * b(iInOffset + 3)
  }
}
      
