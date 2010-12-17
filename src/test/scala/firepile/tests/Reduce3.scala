package firepile.tests

object Reduce3 {

  import firepile._
  import firepile.Device
  import firepile.Arrays._
  import firepile.Spaces._
  import firepile.util.BufferBackedArray._
  import firepile.tree.Trees.Tree
  import com.nativelibs4java.opencl._
  import com.nativelibs4java.util._
  import java.nio.FloatBuffer
  import java.nio.ByteOrder
  import scala.collection.JavaConversions._
  import firepile.Marshaling._
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
  
  var NUM_ITEMS = 16384 // 1048576
  val maxThreads = 512 
  val maxBlocks = 64

  def main(args: Array[String]) = {

  if (args.length > 0) NUM_ITEMS= args(0).toInt
  run
  	
  }
  /*
  def compile[T:FixedSizeMarshal](f: Function2[Array[T],Array[T],Unit]): (Array[T],Array[T]) => Unit = {
    new Function2[Array[T],Array[T],Unit] {
      def apply(a: Array[T], b: Array[T]) = {
        val platform = JavaCL.listPlatforms()(0)
        val devices = platform.listAllDevices(true)
        val context = platform.createContext(null, devices(0))
        val kernStr = new StringBuffer()
        var program: CLProgram = null
        val queue = context.createDefaultQueue()
        val threads = (if (a.length < maxThreads*2) pow(2, ceil(log(a.length) / log(2))) else maxThreads).toInt  // MAX THREADS can be gotten from CL lib
        
        val (kernName: String, tree: List[Tree]) = firepile.Compose.compileToTreeName(f, 2)
        
        for (t: Tree <- tree.reverse)
          kernStr.append(t.toCL)
    
        try {
          program = context.createProgram(kernStr.toString).build
        } catch {
          case e => println(e)
        }

        val kernel = program.createKernel(kernName)

        // Need to use BB arrays here
        val memIn1 = context.createFloatBuffer(CLMem.Usage.Input, a.length)
        val memOut = context.createFloatBuffer(CLMem.Usage.Output, b.length)
        val localMem = new CLKernel.LocalSize(threads * 4L)

        kernel.setArg(0, memIn1)
        kernel.setArg(1, a.length)
        kernel.setArg(2, memOut)
        kernel.setArg(3, b.length)
        kernel.setLocalArg(4, threads * 4L)
        kernel.setArg(5, threads)

        val aBuff = FloatBuffer.allocate(a.length)
        for (i <- 0 until a.length)
          aBuff.put(i, a(i).asInstanceOf[Float])

        memIn1.write(queue, aBuff, true)

        kernel.enqueueNDRange(queue, Array[Int](b.length * threads), Array[Int](threads))

        queue.finish

        val output = NIOUtils.directFloats(b.length, ByteOrder.nativeOrder)
        memOut.read(queue, output, true)

        for (i <- 0 until b.length) 
          b(i) = output.get(i).asInstanceOf[T]
      }
    }
  }
*/

/*
  def sumn[N](A: Array[N])(implicit num: Numeric[N]): N = {
    implicit val gpu: Device = firepile.gpu

    val threads = (if (A.length < gpu.maxThreads*2) pow(2, ceil(log(A.length) / log(2))) else gpu.maxThreads).toInter
    val blocks = ((A.length + (threads * 2 - 1)) / (threads * 2)).toInt
    
    val sumBlockReducer = num match {
      case _: Numeric[Int] => 
        firepile.Compiler.compile {
          (A: Array[Int], B: Array[Int]) => greduce(A, B, A.length, num.plus.asInstanceOf[(Int,Int) => Int], num.zero.asInstanceOf[Int])
        }.asInstanceOf[(Array[N], Array[N]) => Unit]
      case _: Numeric[Float] => 
        firepile.Compiler.compile {
          (A: Array[Float], B: Array[Float]) => greduce(A, B, A.length, num.plus.asInstanceOf[(Float,Float) => Float], num.zero.asInstanceOf[Float])
        }.asInstanceOf[(Array[N], Array[N]) => Unit]
      // want:
        
      //  firepile.Compiler.compile {
      //    (A: Array[N], B: Array[N]) => greduce(A, B, A.length, num.plus, num.zero)
      //  }
        
      // ...
    }
    val B: Array[N] = new Array[N](blocks)

    sumBlockReducer(A, B)
    B.reduceLeft(num.plus)
  }
*/


  def sumint(A: Array[Int]): Int = {
    implicit val gpu: Device = firepile.gpu

    val threads = (if (A.length < gpu.maxThreads*2) pow(2, ceil(log(A.length) / log(2))) else gpu.maxThreads).toInt
    val blocks = ((A.length + (threads * 2 - 1)) / (threads * 2)).toInt
    
    // val add: (Int,Int) => Int = _+_
    val sumBlockReducer: (Array[Int], Array[Int]) => Unit = firepile.Compiler.compile {
      (A: Array[Int], B: Array[Int]) => intreduce(A, B, A.length, _+_, 0)
    }
    val B: Array[Int] = new Array[Int](blocks)

    sumBlockReducer(A, B)
    B.reduceLeft(_+_)
  }


  def sum(A: Array[Float]): Float = {
    implicit val gpu: Device = firepile.gpu

    val threads = (if (A.length < gpu.maxThreads*2) pow(2, ceil(log(A.length) / log(2))) else gpu.maxThreads).toInt
    val blocks = ((A.length + (threads * 2 - 1)) / (threads * 2)).toInt
   
    // val add: (Float, Float) => Float = _+_

    val sumBlockReducer: (Array[Float], Array[Float]) => Unit = firepile.Compiler.compile {
      (A: Array[Float], B: Array[Float]) => reduce(A, B, A.length, _+_, 0f)
    }
    val B: Array[Float] = new Array[Float](blocks)

    sumBlockReducer(A, B)
    B.reduceLeft(_+_)
  }


  def run = {
    val random = new Random(0)
    val randInput = Array.fill(NUM_ITEMS)(random.nextFloat)
    
    // Short term:
    // val sumBlockReducer: (Array[Float], Array[Float]) => Unit = Firepile.compile {
    //     (A: Array[Float], B: Array[Float]) => reduce(A, B, A.length, _+_, 0f)
    // }
    // def sum(A: Array[Float]): Float = {
    //    val B: Array[Float] = new Array[Float](Firepile.numGroups)
    //    sumBlockReducer(A, B)  // parallel + on GPU
    //    B.reduceLeft(_+_) // sequential + on CPU
    // }


    // No term:
    // val blockReducer = Firepile.compile {
    //     (A: Array[Float], B: Array[Float], f: (Float,Float) => Float, z: Float) => reduce(A, B, A.length, f, z)
    // }
    // def reduce(A: Array[Float], f: (Float,Float) => Float, z: Float): Float = {
    //    val B: Array[Float] = new Array[Float](Firepile.numGroups)
    //    blockReducer(A, B, f, z)  // parallel + on GPU; reducer is pre-compiled but not specialized on f
    //    B.foldLeft(z)(f) // sequential + on CPU
    // }
    //
    // Long term:
    // def blockReducer(f: Float,Float) => Float, z: Float) = Firepile.getKernelOrElse((f,z), () => Firepile.compile {
    //     (A: Array[Float], B: Array[Float]) => reduce(A, B, A.length, f, z)
    // })
    // def reduce(A: Array[Float], f: (Float,Float) => Float, z: Float): Float = {
    //    val B: Array[Float] = new Array[Float](Firepile.numGroups)
    //    val k = blockReducer(f, z) // specializes on f and z; compiles NOW (but could memoize to compile only on first invoke for any given f)
    //    k(A, B)  // parallel + on GPU
    //    B.foldLeft(z)(f) // sequential + on CPU
    // }
    // def sum(A: Array[Float]) = reduce(A, _+_, 0f)
    //
    // trait Kernel1[A,B] extends Function1[A,B] {
    //   ??? need to say this.type <: Kx
    //    def composeKernel[C,Kx <: Kernel1[A,B], Ky <: Kernel1[B,C], Kz: Kernel1[A,C]](k: Kx)(implicit composer: KernelComposer[Kx,Ky,Kz]): Kz = composer.compose(this, k)
    // }
    // class MapKernel1[A,B] extends Kernel1[A,B] {
    //    ...
    // }
    // class MapKernelComposer[A,B,C] extends KernelComposer[MapKernel1[A,B], MapKernel1[B,C], MapKernel1[A,C]] {
    //    def compose(kx: MapKernel1[A,B], ky: MapKernel1[B,C]): MapKernel1[A,C]
    //    ...
    // }
    // trait Kernel2[A1,A2,B] extends Function2[A1,A2,B] {
    // }

    val cpuSum = randInput.sum
    val gpuSum = sum(randInput)

    println("CPU sum = " + cpuSum + "   GPU sum = " + gpuSum)
/*
    val random2 = new Random(100)

    val randInput2 = Array.fill(NUM_ITEMS)(random2.nextFloat)
    val gpuSum2 = sum(randInput2)
   
    val randInput3 = Array.fill(NUM_ITEMS)(random.nextInt(100))
    val gpuSum3 = sumint(randInput3)

    println("CPU sum = " + cpuSum + "   GPU sum = " + gpuSum + "\nCPU sum2 = " + randInput2.sum + "   GPU sum2 = " + gpuSum2)

    println("GPU sum3 (Int) = " + gpuSum3)
*/
    /*
    firepile.Compose.compileToTree(
      (A: Array[Float], B: Array[Float], z: Float, f: (Float,Float)=>Float) => reduce(A, B, A.length, f, z), 2)
    */
  }


 
  /* Uses n/2 threads, performs the the first level of reduction when
     reading from global memory
     n - number of elements to reduce
  */
  // @kernel(numGroups = odata.length, numItems = idata.length)
  // @where(n <= numItems)
  /* @kernel("(__global float *idata, __global float *odata, int n, float z, __local float *sdata)") */
  def greduce[A](idata: Array[A], odata: Array[A], n: Int, f: (A,A) => A, z: A) =
      (id: Id1, sdata: Array[A]) => {
    // perform first level of reduction reading from global memory, writing to shared memory
    val tid = id.local.toInt

    // i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    // (row=group, col=local, rowlength=localSize*2)
    // something like:
    // IdSpace(id.numGroups, localSize*2).index(id.group, id.local).toInt
    // Oy!

    val i = id.group * (id.config.localSize*2) + id.local

    /*
    val grpId = id.group.toInt
    val lclSize2 = id.config.localSize.toInt * 2
    val lclId = id.local.toInt
    val i = grpId * lclSize2 + lclId
    */


    sdata(tid) = if (i < n) idata(i) else z

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

  /* Uses n/2 threads, performs the the first level of reduction when
     reading from global memory
     n - number of elements to reduce
  */
  // @kernel(numGroups = odata.length, numItems = idata.length)
  // @where(n <= numItems)
  /* @kernel("(__global float *idata, __global float *odata, int n, float z, __local float *sdata)") */
  def reduce(idata: Array[Float], odata: Array[Float], n: Int, f: (Float,Float) => Float, z: Float) =
      (id: Id1, sdata: Array[Float]) => {
    // perform first level of reduction reading from global memory, writing to shared memory
    val tid = id.local.toInt

    // i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    // (row=group, col=local, rowlength=localSize*2)
    // something like:
    // IdSpace(id.numGroups, localSize*2).index(id.group, id.local).toInt
    // Oy!

    val i = id.group * (id.config.localSize*2) + id.local

    /*
    val grpId = id.group.toInt
    val lclSize2 = id.config.localSize.toInt * 2
    val lclId = id.local.toInt
    val i = grpId * lclSize2 + lclId
    */


    sdata(tid) = if (i < n) idata(i) else z

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

  def intreduce(idata: Array[Int], odata: Array[Int], n: Int, f: (Int,Int) => Int,  z: Int) =
      (id: Id1, sdata: Array[Int]) => {
    // perform first level of reduction reading from global memory, writing to shared memory
    val tid = id.local.toInt

    // i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    // (row=group, col=local, rowlength=localSize*2)
    // something like:
    // IdSpace(id.numGroups, localSize*2).index(id.group, id.local).toInt
    // Oy!

    val i = id.group * (id.config.localSize*2) + id.local

    /*
    val grpId = id.group.toInt
    val lclSize2 = id.config.localSize.toInt * 2
    val lclId = id.local.toInt
    val i = grpId * lclSize2 + lclId
    */


    sdata(tid) = if (i < n) idata(i) else z

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
