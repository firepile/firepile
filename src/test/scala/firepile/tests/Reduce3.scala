package firepile.tests

object Reduce3 {

  import firepile._
  import firepile.Device
  import firepile.Arrays._
  import firepile.Spaces._
  import firepile.Args._
  import firepile.util.BufferBackedArray._

  class size(exp: Int) extends scala.StaticAnnotation { }
  class local extends scala.StaticAnnotation { }

/*
  implicit def arrayReducer(a: Array[Float]) = new {
    def reduceBlock(z: Float)(f: (Float,Float) => Float) = {
      assert(a.length <= 128)
      val apad = a padTo (128,z)
      val bpad = Array.ofDim[Float](128)
      import firepile.Compose.spawn
      spawn {
        reduce(apad, bpad, a.length, f, z)
      }
      bpad take a.length
    }
  }

  def test = {
    val A = Array.tabulate[Float](100)(x => x)
    val B = A.reduceBlock(0.f)(_+_)
    println(B)
  }
*/

  def main(args: Array[String]) = compile

  def compile = {
    firepile.Compose.compileToTree(
      (A: Array[Float], B: Array[Float]) => reduce(A, B, A.length, _+_, 0f), 2)
  }


  object localMem { def barrier = () }

  /* Uses n/2 threads, performs the the first level of reduction when
     reading from global memory
     n - number of elements to reduce
  */
  // @kernel(numGroups = odata.length, numItems = idata.length)
  // @where(n <= numItems)
  def reduce(idata: Array[Float], odata: Array[Float], n: Int, f: (Float,Float) => Float, z: Float) =
      (id: Id1, sdata: Array[Float] @local) => {
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

    localMem.barrier 

    // do reduction in shared memory
    // byfun -> applying? byfunc?
    for (s <- id.config.localSize / 2 until 0 byfun (_/2)) {
      if (tid < s)
        sdata(tid) = f(sdata(tid), sdata(tid + s))
      localMem.barrier
    }

    // write results back to global
    if (tid == 0) 
      odata(id.group) = sdata(0)
  }
}
