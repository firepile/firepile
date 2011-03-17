 package firepile.compiler.collections
 
 import scala.math.{ceil, pow, log}
 import firepile.util.BufferBackedArray._
 import firepile.Marshaling._
 import firepile.Spaces._
 import firepile.tree.Trees._
 import firepile.Implicits._
 import firepile.Device
 import firepile.Compiler._
 class size(exp: Int) extends scala.StaticAnnotation { }
 class local extends scala.StaticAnnotation { }
  
 object GPUArray {
   // This will be compiled into a kernel specialized on f and A and B.
   //  def map(a: BBArray[A], b: BBArray[B], f: A => B) = { 
      /*
      def map(A: Array[Float]): Float = {
	   val NUM_ITEMS = 16384 // 1048576
	   val maxThreads = 512 
	   val maxBlocks = 64
           implicit val gpu: Device = firepile.gpu

	       val threads = (if (A.length < gpu.maxThreads*2) pow(2, ceil(log(A.length) / log(2))) else gpu.maxThreads).toInt
	       val blocks = ((A.length + (threads * 2 - 1)) / (threads * 2)).toInt
	       println("BLOCKS = " + blocks)
	       val mapper: (Array[Float], Array[Float]) => Unit = firepile.Compiler.compile {
		 (A: Array[Float], B: Array[Float]) => map(A, B, A.length, *, 2)
	       }
	       val B: Array[Float] = new Array[Float](blocks)
	       mapper(A, B)
     }
     */

     /*
     def reduce(A: Array[Float]): Float = {
         implicit val gpu: Device = firepile.gpu
     
         val threads = (if (A.length < gpu.maxThreads*2) pow(2, ceil(log(A.length) / log(2))) else gpu.maxThreads).toInt
         val blocks = ((A.length + (threads * 2 - 1)) / (threads * 2)).toInt
         println("BLOCKS = " + blocks)
         val sumBlockReducer: (Array[Float], Array[Float]) => Unit = firepile.Compiler.compile {
           (A: Array[Float], B: Array[Float]) => reduceKernel(A, B, A.length, _+_, 0f)
         }
         val B: Array[Float] = new Array[Float](blocks)
     
         sumBlockReducer(A, B)
         B.reduceLeft(_+_)
     }
      object localMem {  def barrier = () }
      
      import localMem._ 
      
      def reduceKernel(idata: Array[Float], odata: Array[Float], n: Int, f: (Float,Float) => Float, z: Float) =
           (id: Id1, sdata: Array[Float]) => {
         // perform first level of reduction reading from global memory, writing to shared memory
         val tid = id.local.toInt
         val i = id.group * (id.config.localSize*2) + id.local
         
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
  
     /*
     def blockReduce(a: BBArray[A], b: BBArray[B], f: (A,A) => A) = {  }
  
     def mapKernel(f: A=>B): Kernel2[BBArray[A], BBArray[B]] = { } 
     */
   }
   */

/*
   class GPUArray[A](a: BBArray[A]) {

   /*   
     def map(f: A => B)(implicit dev: Device) = {
      
       //val k = /* memoize */ dev.compile( (a:BBArray[A], b:BBArray[B]) => GPUArray.map(a, b, f) )
       //val that = BBArray.ofDim[B](a.length)
       //k(this, that)
       //new GPUArray(that)
       
     }
     def reduce(f: (A,A) => A)(implicit dev: Device) /* ??? (implicit blockSize: Int) */ = {
       //val that = blockReduce(f)
       //that.reduceLeft(f)
     }
     def blockReduce(f: (A,A) => A)(implicit dev: Device) /* ??? (implicit blockSize: Int) */ = {
       //val k = /* memoize */ dev.compile( (a:BBArray[A], b:BBArray[A]) => GPUArray.blockReduce(a, b, f) )
       //val that = BBArray.ofDim[B](a.length / blockSize)
       //k(this, that)
       //new GPUArray(that)
     }
     
     */
*/    
   }

  
  /*
   //kinda want this:
   trait Kernel1[A,B] extends Function1[A,B]
   val k = mapk(_*2) compose reducek(_+_)
   k(a, b)
  
   val a = BBArray.tabulate[Float](1000000)(_.toFloat)
   val g = GPUArray(a, dev)
   val b = g.map(_*2).reduce(_+_)
  
   g.map returns a MapKernel1
  
  
   val k1 = dev.compile( ... GPUArray.map(.., _*2) )
   val k2 = dev.compile( ... GPUArray.blockReduce(.., _+_) )
   
   */
   

  
  
