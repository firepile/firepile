object Reduce3 {

  /* Uses n/2 threads, performs the the first level of reduction when
     reading from global memory

     n - number of elements to reduce
  */
  def reduce3Kernel(idata: Array[Float], odata: Array[Float], n: UInt, sdata: @local Array[Float])(id: Id, localMem: LocalMem) = {
    // perform first level of reduction reading from global memory, writing to shared memory
    val tid: UInt = id.local

    // i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);
    val i: UInt = id.group * (id.localSize*2) + id.local

    sdata(tid) = if (i < n) idata(i) else 0
    if (i + id.localSize < n)
      sdata(tid) = idata(i + id.localSize)

    localMem.barrier
    // what if we called barrier on the local objects instead?
    // sdata.barrier

    // do reduction in shared memory
    for ( s: UInt <- id.localSize / 2 until 0 by (s >>= 1)) {
      if (tid < s)
        sdata(tid) += sdata(tid + s)
      localMem.barrer
    }

    // write results back to global
    if (tid == 0) 
      odata(id.group) = sdata(0)
  }
