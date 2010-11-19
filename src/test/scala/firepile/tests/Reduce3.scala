package firepile.tests

object Reduce3 {

  import firepile._
  import firepile.Device
  import firepile.Arrays._
  import firepile.Spaces._
  import firepile.Args._
  import firepile.util.BufferBackedArray._
  import com.nativelibs4java.opencl._
  import com.nativelibs4java.util._
  import java.nio.FloatBuffer
  import java.nio.ByteOrder
  import scala.collection.JavaConversions._
  import scala.util.Random
  import scala.math.{ceil, pow, log}

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
  
  val NUM_ITEMS = 1048576
  val maxThreads = 64
  val maxBlocks = 64

  def main(args: Array[String]) = compile

  def compile = {
    val randInput = Array.fill(NUM_ITEMS)(Random.nextFloat)
    val kernelStr = new StringBuffer()
    val platform = JavaCL.listPlatforms()(0)
    val devices = platform.listAllDevices(true)

    val context = platform.createContext(null, devices(0))

    val (_,tree) = firepile.Compose.compileToTree(
      (A: Array[Float], B: Array[Float]) => reduce(A, B, A.length, _+_, 0f), 2)

    for (t <- tree.reverse) {
      kernelStr.append(t.toCL)
    }

    println("-------------")
    println(kernelStr.toString)

    var program: CLProgram = null
    try {
      program = context.createProgram(kernelStr.toString).build
    } catch {
      case e => println(e)
    }

    val kernel = program.createKernel("firepile_tests_Reduce3__anonfun_2apply")
    val queue = context.createDefaultQueue()

    val threads = if (NUM_ITEMS < maxThreads*2) pow(2, ceil(log(NUM_ITEMS) / log(2))) else maxThreads
    val blocks = ((NUM_ITEMS + (threads * 2 - 1)) / (threads * 2)).toInt

    val outputData = new Array[Float](blocks)
    
    val memIn1 = context.createFloatBuffer(CLMem.Usage.Input, NUM_ITEMS * 4)
    val memOut = context.createFloatBuffer(CLMem.Usage.Output, blocks * 4)
    val localMem = new CLKernel.LocalSize(threads.toLong * 4L)

    kernel.setArgs(memIn1, NUM_ITEMS.asInstanceOf[AnyRef], memOut, blocks.asInstanceOf[AnyRef], localMem, threads.toInt.asInstanceOf[AnyRef])

    val a = FloatBuffer.allocate(NUM_ITEMS * 4)
    for (rNum <- randInput)
      a.put(rNum)

    memIn1.write(queue, a, true)

    kernel.enqueueNDRange(queue, Array[Int](blocks), Array[Int](threads.toInt))

    queue.finish

    val output = NIOUtils.directFloats(blocks * threads.toInt, ByteOrder.nativeOrder)
    memOut.read(queue, output, true)

    /*
    firepile_tests_Reduce3__anonfun_1apply(__global float* _arg0_data, __global int _arg0_len, __global float* _arg1_data, __global int _arg1_len, __local float* _arg1_c_data, __local int _arg1_c_len)
    */



    /*
    firepile.Compose.compileToTree(
      (A: Array[Float], B: Array[Float], z: Float, f: (Float,Float)=>Float) => reduce(A, B, A.length, f, z), 2)
    */
  }


  object localMem { def barrier = () }

  /* Uses n/2 threads, performs the the first level of reduction when
     reading from global memory
     n - number of elements to reduce
  */
  // @kernel(numGroups = odata.length, numItems = idata.length)
  // @where(n <= numItems)
  /* @kernel("(__global float *idata, __global float *odata, int n, float z, __local float *sdata)") */
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
    var s = id.config.localSize / 2
    while (s > 0) {
      if (tid < s)
        sdata(tid) = f(sdata(tid), sdata(tid + s))
      localMem.barrier
      s /= 2
    }

    // write results back to global
    if (tid == 0) 
      odata(id.group) = sdata(0)
  }
}
