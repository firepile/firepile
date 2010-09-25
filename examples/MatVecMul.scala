object MatVecMul {
  def main(args: Array[String]) = {
    val a = Array(Array[Float](3.1f, 2.7f, 8.2f),
                  Array[Float](11.6f, 9.2f, 23.1f),
                  Array[Float](12.8f, 15.7f, 22.1f))
    val b = Array[Float](7.1f, 11.5f, 78.3f)

    val c = matVecMulHost(a, b)

    c.foreach(i => println(i))

  }

  def matVecMulHost(mat: Array[Array[Float]], vec: Array[Float]): Array[Float] = {
    val res = new Array[Float](mat.length)

    for (i <- 0 until mat.length) {
      var sum = 0.0f
      for (j <- 0 until vec.length)
        sum += mat(i)(j) * vec(j)
      res(i) = sum
    }

    res
  }

  def matVecMulCoalesced0(/* const global */ mat: Array[Array[Float]], /* const global */ vec: Array[Float], /* local */partialDotProduct: Array[Float]): Array[Float] = {
    val res = new Array[Float](mat.length)

    for (y <- get_group_id(0) until (mat.length, get_num_groups(0))) {
      val row = y    // row = M + y * width
      var sum = 0.0f

      for (x <- get_local_id(0) until (vec.length, get_local_size(0)))
        sum += mat(row)(x) * vec(x)

      partialDotProduct(get_local_id(0)) = sum

      // barrier(CLK_LOCAL_MEM_FENCE)

      // first work item adds all partial dot products, writes result in global result
      if (get_local_id(0) == 0) {
        var dotProduct = 0.0f

        for (i <- 0 until get_local_size(0))
          dotProduct += partialDotProduct(i)

        res(y) = dotProduct
      }

    // barrier(CLK_LOCAL_MEM_FENCE)
    }

    res
  }

  // see Spaces.scala
  case class Id1(global: Int, group: Int, local: Int, localSize: Int, numGroups: Int)
  case class Id2(global: (Int,Int), group: (Int,Int), local: (Int,Int), localSize: (Int,Int), numGroups: (Int,Int))

  /** Array indexed by work item id (id.local) */
  trait LocalArray[Pt<:Point[Pt], A] {
    def apply(i: Pt): A
    def update(i: Pt, v: A): Unit
  }

  trait LocalMem {
    def barrier: Unit
  }

  type Array1[A] = Array[A]
  trait Array2[A] extends Function2[Int,Int,A] { /* ... */ }

  // gpu.spawn { matVecMulCoalesced0b(mat, vec) _ }

  def matVecMulCoalesced0b(/* const global */ mat: Array[Array[Float]], /* const global */ vec: Array[Float])(/* local */partialDotProduct: Array[Float])(id: Id1, localMem: LocalMem): Array[Float] = {
    val res = new Array[Float](mat.length)

    // perhaps something like:
    for (y <- id.group until mat.length by id.numGroups) {
      res(y) = (mat(y), vec).byGroup.zipWith(_*_).cacheLocally.reduce(0.0f)(_+_)
      // OR:
      res(y) = (mat(y), vec).byGroup.zipWith(_*_).sum
    }

    // worse:
    for (y <- id.group until mat.length by id.numGroups) {
      val row = y    // row = M + y * width
      var sum = 0.0f

      for (x <- id.local until vec.length by id.localSize)
        sum += mat(row)(x) * vec(x)

      partialDotProduct(id.local) = sum

      localMem.barrier

      // first work item adds all partial dot products, writes result in global result
      if (id.local == 0) {
        res(y) = partialDotProduct.sum  /* pdp.reduce(0.0f)(_+_) */
      }

      localMem.barrier

    }

    res
  }

  /*
        for (uint stride = get_local_size(0) / 2; stride > 0; stride /= 2) {
            ...
        }
        OR:
        for (stride <- localMem.strides) {
        }
        OR:
        for (stride <- (id.localSize / 2) halvingUntil 0) {
        }
        OR:
        for (stride <- 0 doublingTo id.localSize) {
        }
        OR:
        for (stride <- (id.localSize / 2) until 0 by (_/2)) {
        }
  */

  def get_group_id(i: Int): Int = i

  def get_num_groups(i: Int): Int = i
  
  def get_local_size(i: Int): Int = i

  def get_local_id(i: Int): Int = i
}
