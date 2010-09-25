object Reduce3 {

  class size(exp: Int) extends scala.StaticAnnotation { }
  class local extends scala.StaticAnnotation { }

  import firepile.Spaces._

  /* Uses n/2 threads, performs the the first level of reduction when
     reading from global memory
     n - number of elements to reduce
  */
  // @kernel(numGroups = odata.length, numItems = idata.length)
  // @where(n <= numItems)
  def reduce(idata: Array[Float], odata: Array[Float], n: Int, f: (Float,Float) => Float, z: Float) =
      (id: Id1, sdata: /* @local */ Array[Float]) => {
    // perform first level of reduction reading from global memory, writing to shared memory
    val tid = id.local.toInt

    // i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    // (row=group, col=local, rowlength=localSize*2)
    // something like:
    // IdSpace(id.numGroups, localSize*2).index(id.group, id.local).toInt
    // Oy!
    val i = id.group * (id.config.localSize*2) + id.local

    val ii = if (i < n) idata(i) else z
    if (i + id.config.localSize < n)
      sdata(tid) = f(sdata(tid), idata(i + id.config.localSize))

    // localMem.barrier  ERROR

    // do reduction in shared memory
    // byfun -> applying? byfunc?
    for (s <- id.config.localSize / 2 until 0 byfun (_/2)) {
      if (tid < s)
        sdata(tid) = f(sdata(tid), sdata(tid + s))
      // localMem.barrer  ERROR
    }

    // write results back to global
    if (tid == 0) 
      odata(id.group) = sdata(0)
  }
}
