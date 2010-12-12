package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.Spaces._
import firepile.tree.Trees._
import firepile.Implicits._

import com.nativelibs4java.opencl.CLMem
import com.nativelibs4java.opencl.CLKernel

import compiler.JVM2CL.compileRoot
import compiler.JVM2CL.mangleName
import compiler.JVM2CL.methodName

import java.nio.ByteBuffer

// TODO: remove most of this.
object Compiler {
  val Header = ("\n" +
    "struct Point1 {                                                                               \n" +
    "  int x;                                                                                      \n" +
    "};                                                                                            \n" +
    "                                                                                              \n" +
    "struct Point2 {                                                                               \n" +
    "  int x;                                                                                      \n" +
    "  int y;                                                                                      \n" +
    "};                                                                                            \n" +
    "                                                                                              \n" +
    "struct Point3 {                                                                               \n" +
    "  int x;                                                                                      \n" +
    "  int y;                                                                                      \n" +
    "  int z;                                                                                      \n" +
    "};                                                                                            \n" +
    "                                                                                              \n" +
    "#define id1_threadIndex(p) p.x                                                                \n" +
    "#define id2_threadIndex(p) (p.x * get_global_size(0) + p.y)                                   \n" +
    "#define id3_threadIndex(p) (((p.x * get_global_size(0) + p.y) * get_global_size(1)) + p.z)    \n" +
    "                                                                                              \n" +
    "#define id1_blockIndex(p) p.x                                                                 \n" +
    "#define id2_blockIndex(p) (p.x * get_num_groups(0) + p.y)                                     \n" +
    "#define id3_blockIndex(p) (((p.x * get_num_groups(0) + p.y) * get_num_groups(1)) + p.z)       \n" +
    "                                                                                              \n" +
    "#define id1_localThreadIndex(p) p.x                                                           \n" +
    "#define id2_localThreadIndex(p) (p.x * get_local_size(0) + p.y)                               \n" +
    "#define id3_localThreadIndex(p) (((p.x * get_local_size(0) + p.y) * get_local_size(1)) + p.z) \n" +
    "                                                                                              \n" +
    "#define id1_thread  { get_global_id(0) }                                                      \n" +
    "#define id2_thread  { get_global_id(0), get_global_id(1) }                                    \n" +
    "#define id3_thread  { get_global_id(0), get_global_id(1), get_global_id(2) }                  \n" +
    "                                                                                              \n" +
    "#define id1_block  { get_group_id(0) }                                                        \n" +
    "#define id2_block  { get_group_id(0), get_group_id(1) }                                       \n" +
    "#define id3_block  { get_group_id(0), get_group_id(1), get_group_id(2) }                      \n" +
    "                                                                                              \n" +
    "#define id1_localThread  { get_local_id(0) }                                                  \n" +
    "#define id2_localThread  { get_local_id(0), get_local_id(1) }                                 \n" +
    "#define id3_localThread  { get_local_id(0), get_local_id(1), get_local_id(2) }                \n" +
    "                                                                                              \n" +
    "#define ARRAY_TYPE(Q,T) T ## Q ## _array                                                      \n" +
    "#define ARRAY_DECL(Q,T) typedef struct { const int length; __ ## Q T *a; } ARRAY_TYPE(Q,T);   \n" +
    "                                                                                              \n" +
    "#define ARRAY_DECLS(Q)                                                                      \\\n" +
    "ARRAY_DECL(Q, char)                                                                         \\\n" +
    "ARRAY_DECL(Q, short)                                                                        \\\n" +
    "ARRAY_DECL(Q, ushort)                                                                       \\\n" +
    "ARRAY_DECL(Q, int)                                                                          \\\n" +
    "ARRAY_DECL(Q, long)                                                                         \\\n" +
    "ARRAY_DECL(Q, float)                                                                        \\\n" +
    "ARRAY_DECL(Q, double)                                                                         \n" +
    "                                                                                              \n" +
    "ARRAY_DECLS(constant)                                                                         \n" +
    "ARRAY_DECLS(global)                                                                           \n" +
    "ARRAY_DECLS(local)                                                                            \n" +
    "ARRAY_DECLS(private)                                                                          \n" +
    "                                                                                              \n" +
    "\n")

  def generateMapKernel1(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, List(Formal(formal, _)), _) => ("\n" +
      "inline " + tree.toCL + "\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "  __global const " + formal.toCL + "* a,                                                        \n" +
      "  const int a_len,                                                                          \n" +
      "  __global " + returnType.toCL + "* output,                                                 \n" +
      "  const int output_len)                                                                     \n" +
      "{                                                                                           \n" +
      "  int i = get_global_id(0);                                                                 \n" +
      "  if (i < output_len)                                                                       \n" +
      "    output[i] = " + name + "(a[i]);                                                         \n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  def generateMapKernel2(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, List(Formal(formal1, _), Formal(formal2, _)), _) => ("\n" +
      "inline " + tree.toCL + "\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "   __global const     " + formal1.toCL + "* a,                                                  \n" +
      "   const int a_len,                                                                         \n" +
      "   __global const     " + formal2.toCL + "* b,                                                  \n" +
      "   const int b_len,                                                                         \n" +
      "   __global " + returnType.toCL + "* output,                                                \n" +
      "   const int output_len)                                                                    \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   if (i < output_len)                                                                      \n" +
      "     output[i] = " + name + "(a[i], b[i]);                                                  \n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  def typeSig(t: java.lang.Class[_]): String = t match {
    case t if t == java.lang.Boolean.TYPE => "Z"
    case t if t == java.lang.Byte.TYPE => "B"
    case t if t == java.lang.Short.TYPE => "S"
    case t if t == java.lang.Character.TYPE => "C"
    case t if t == java.lang.Integer.TYPE => "I"
    case t if t == java.lang.Long.TYPE => "L"
    case t if t == java.lang.Float.TYPE => "F"
    case t if t == java.lang.Double.TYPE => "D"
    case t if t == java.lang.Void.TYPE => "V"
    case t if t.isArray => "[" + typeSig(t.getComponentType)
    case t => "L" + t.getName.replace('.', '/') + ";"
  }

  def signature(m: java.lang.reflect.Method) =
    m.getName + "(" + m.getParameterTypes.toList.map(t => typeSig(t)).mkString("") + ")" + typeSig(m.getReturnType)

  def findApplyMethod(src: AnyRef, arity: Int): java.lang.reflect.Method = {
    val k = src.getClass
    for (m <- k.getDeclaredMethods) {
      if (m.getParameterTypes.length == arity)
        if (m.getName.startsWith("apply$mc") && m.getName.endsWith("$sp"))
          return m
    }
    for (m <- k.getDeclaredMethods) {
      if (m.getParameterTypes.length == arity)
        if (m.getName.equals("apply"))
          return m
    }
    throw new RuntimeException("Could not find apply/" + arity + " method in " + k.getName)
  }

  def compileMapKernel1(src: Function1[_,_], kernelName: String): String = {
    val k = src.getClass
    val apply = findApplyMethod(src, 1)
    val funs = compileRoot(k.getName, signature(apply)).reverse

    if (funs.isEmpty)
      throw new RuntimeException("Could not compile method in " + k.getName + "." + apply.getName)

    funs.map {
      case fd @ FunDef(returnType, name, List(_), _) if (name.equals(methodName(apply))) => generateMapKernel1(fd, kernelName)
      case fd => fd.toCL + "\n"
    }.mkString("\n")
  }

  def compileMapKernel2(src: Function2[_,_,_], kernelName: String): String = {
    val k = src.getClass
    val apply = findApplyMethod(src, 2)
    val funs = compileRoot(k.getName, signature(apply)).reverse

    if (funs.isEmpty)
      throw new RuntimeException("Could not compile method in " + k.getName + "." + apply.getName)

    funs.map {
      case fd @ FunDef(returnType, name, List(_, _), _) if (name.equals(methodName(apply))) => generateMapKernel2(fd, kernelName)
      case fd => fd.toCL + "\n"
    }.mkString("\n")
  }

  def compileReduceKernel1(src: Function2[_,_,_], kernelName: String): String =
    generateReduceKernel1(FunDef(ValueType("float"), Id("apply"), List(Formal(ValueType("float"), Id("x")), Formal(ValueType("float"), Id("y"))),
                                                              Return(Bin(Id("x"), "+", Id("y")))), kernelName)

  def compileReduceKernel1(src: Function1[_,Function1[_,_]], kernelName: String): String =
    generateReduceKernel1(FunDef(ValueType("float"), Id("apply"), List(Formal(ValueType("float"), Id("x")), Formal(ValueType("float"), Id("y"))),
                                                              Return(Bin(Id("x"), "+", Id("y")))), kernelName)

  def generateReduceKernel1(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, List(Formal(formal1, _), Formal(formal2, _)), _) => ("\n" +
      "inline " + tree.toCL + "\n" +
"#define T " + returnType.toCL + "                                                  \n" +
"#define blockSize 128                                                              \n" +
"__kernel void " + kernelName + "(                                                  \n" +
"  __global const     T *g_idata, /* thread indexed */                                  \n" +
"  const int g_idata_length,                                                        \n" +
"  __global T *g_odata,       /* block indexed */                                   \n" +
"  const int g_odata_length,                                                        \n" +
"  __local T* sdata,          /* local thread indexed */                            \n" +
"  const int sdata_length) {                                                        \n" +
"   // perform first level of reduction,                                            \n" +
"   // reading from global memory, writing to local memory                          \n" +
"   unsigned int n = g_idata_length;                                                \n" +
"   unsigned int tid = get_local_id(0);                                             \n" +
"   unsigned int i = get_global_id(0);                                              \n" +
"                                                                                   \n" +
"   sdata[tid] = (i < n) ? g_idata[i] : 0;                                          \n" +
"                                                                                   \n" +
"   barrier(CLK_LOCAL_MEM_FENCE);                                                   \n" +
"                                                                                   \n" +
"   // do reduction in shared mem                                                   \n" +
"   #pragma unroll 1                                                                \n" +
"   for(unsigned int s=get_local_size(0)/2; s>0; s>>=1)                             \n" +
"   {                                                                               \n" +
"       if (tid < s && i+s < n)                                                     \n" +
"       {                                                                           \n" +
"           sdata[tid] = " + name + "(sdata[tid], sdata[tid+s]);                    \n" +
"       }                                                                           \n" +
"       barrier(CLK_LOCAL_MEM_FENCE);                                               \n" +
"   }                                                                               \n" +
"                                                                                   \n" +
"   // write result for this block to global mem                                    \n" +
"   if (tid == 0)                                                                   \n" +
"       g_odata[get_group_id(0)] = sdata[0];                                        \n" +
"}                                                                                  \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  var next = 0
  def freshName(base: String = "tmp") = {
    next += 1
    base + next
  }

  trait Kernel
  trait Kernel1[A] extends Function1[A,Unit] with Kernel
  trait Kernel2[A1,A2] extends Function2[A1,A2,Unit] with Kernel
  trait Kernel3[A1,A2,A3] extends Function3[A1,A2,A3,Unit] with Kernel
  trait Kernel4[A1,A2,A3,A4] extends Function4[A1,A2,A3,A4,Unit] with Kernel

  def compile[A](f: A => Unit)(implicit ma: Marshal[A], dev: Device): Kernel1[A] = throw new RuntimeException("unimplemented")
  // e.g., reduce(input: Array[Int], output: Array[Int])
  // e.g., map(input: Array[Int], output: Array[Float])

  import scala.collection.mutable.HashMap
  val kernelCache = new HashMap[AnyRef, Kernel]
  // [NN] move to Device?
  def memoize[A1,A2](f: (A1,A2) => Unit)(k: => Kernel2[A1,A2]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel2[A1,A2]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1,A2,A3](f: (A1,A2,A3) => Unit)(k: => Kernel3[A1,A2,A3]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel3[A1,A2,A3]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1,A2,A3,A4](f: (A1,A2,A3,A4) => Unit)(k: => Kernel4[A1,A2,A3,A4]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel4[A1,A2,A3,A4]) =>
        println("found kernel in cache")
        k2
    }
  }

  def compile[A1,A2](f: (A1,A2) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], dev: Device): Kernel2[A1,A2] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = firepile.Compose.compileToTreeName(f, 2)
        
    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)

    new Kernel2[A1,A2] {
      def apply(a1: A1, a2: A2): Unit = apply(a1, a2, -1, -1) 
      def apply(a1: A1, a2: A2, globalWkSize: Int, localWkSize: Int): Unit = { 
        val bufA1: ByteBuffer = transA1.toBuffer(a1).head
        val bufA2: ByteBuffer = transA2.toBuffer(a2).head
        
        val numItemsA1 = bufA1.capacity / sizeA1 
        val numItemsA2 = bufA2.capacity / sizeA2 

        val bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
        // val bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1.capacity)
        val bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA2.capacity)
        
        val threads = (if (numItemsA1 < dev.maxThreads*2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt 

        kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
        kernBin.setArg(1, numItemsA1)
        kernBin.setArg(2, bufA2CLBuf)
        kernBin.setArg(3, numItemsA2)

        if (globalWkSize == -1 && localWkSize == -1) {
          kernBin.setLocalArg(4, threads * sizeA1)
          kernBin.setArg(5, threads)
          kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA2 * threads), Array[Int](threads))
        }
        else {
          kernBin.setLocalArg(4, localWkSize * sizeA1)
          kernBin.setArg(5, localWkSize)
          kernBin.enqueueNDRange(dev.queue, Array[Int](globalWkSize), Array[Int](localWkSize))
        }

        dev.queue.finish

        val bufOut = allocDirectBuffer(bufA2.capacity)
        bufA2CLBuf.read(dev.queue, bufOut, true)

        bufOut.rewind

        // [NN] maybe need to copy?  but, probably not
        Array.copy(transA2.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a2.asInstanceOf[AnyRef], 0, numItemsA2)
      }
    }
  }

  def compile[A1,A2,A3](f: (A1,A2,A3) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], dev: Device) = throw new RuntimeException("compile3 unimplmented")

  def compile[A1,A2,A3,A4](f: (A1,A2,A3,A4) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], dev: Device): Kernel4[A1,A2,A3,A4] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val transA4 = implicitly[Marshal[A4]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val sizeA4 = transA4.sizes(1).head
    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = firepile.Compose.compileToTreeName(f, 4)
        
    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)

    new Kernel4[A1,A2,A3,A4] {
      def apply(a1: A1, a2: A2, a3: A3, a4: A4): Unit = apply(a1, a2, a3, a4, -1, -1) 
      def apply(a1: A1, a2: A2, a3: A3, a4: A4, globalWkSize: Int, localWkSize: Int): Unit = { 
        val bufA1: ByteBuffer = transA1.toBuffer(a1).head
        val bufA2: ByteBuffer = transA2.toBuffer(a2).head
        val bufA3: ByteBuffer = transA3.toBuffer(a3).head
        val bufA4: ByteBuffer = transA4.toBuffer(a4).head
        
        val numItemsA1 = bufA1.capacity / sizeA1 
        val numItemsA2 = bufA2.capacity / sizeA2 
        val numItemsA3 = bufA3.capacity / sizeA3 
        val numItemsA4 = bufA4.capacity / sizeA4 

        val bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
        val bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
        val bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA3, true)
        val bufA4CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA4.capacity)
        
        val threads = (if (numItemsA1 < dev.maxThreads*2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt 

        kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
        kernBin.setArg(1, numItemsA1)
        kernBin.setArg(2, bufA2CLBuf)
        kernBin.setArg(3, numItemsA2)
        kernBin.setArg(4, bufA3CLBuf)
        kernBin.setArg(5, numItemsA3)
        kernBin.setArg(6, bufA4CLBuf)
        kernBin.setArg(7, numItemsA4)

        if (globalWkSize == -1 && localWkSize == -1) {
          kernBin.setLocalArg(8, threads * sizeA1)
          kernBin.setArg(9, threads)
          kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA2 * threads), Array[Int](threads))
        }
        else {
          // We don't really know if the local item types are the same as the global item types
          kernBin.setLocalArg(8, localWkSize * sizeA1)
          kernBin.setArg(9, localWkSize)
          kernBin.enqueueNDRange(dev.queue, Array[Int](globalWkSize), Array[Int](localWkSize))
        }

        dev.queue.finish

        val bufOut = allocDirectBuffer(bufA4.capacity)
        bufA4CLBuf.read(dev.queue, bufOut, true)

        bufOut.rewind

        // [NN] maybe need to copy?  but, probably not
        Array.copy(transA4.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a4.asInstanceOf[AnyRef], 0, numItemsA4)
      }
    }
  }



  // ...

  // TODO:
  // Write:
  // object GPUArray {
  //   // This will be compiled into a kernel specialized on f and A and B.
  //   def map(a: BBArray[A], b: BBArray[B], f: A => B) = { ... }
  //   def blockReduce(a: BBArray[A], b: BBArray[B], f: (A,A) => A) = { ... }
  //
  //   def mapKernel(f: A=>B): Kernel2[BBArray[A], BBArray[B]]
  // }
  //
  // class GPUArray[A](a: BBArray[A]) {
  //   def map(f: A => B)(implicit dev: Device) = {
  //     val k = /* memoize */ dev.compile( (a:BBArray[A], b:BBArray[B]) => GPUArray.map(a, b, f) )
  //     val that = BBArray.ofDim[B](a.length)
  //     k(this, that)
  //     new GPUArray(that)
  //   }
  //   def reduce(f: (A,A) => A)(implicit dev: Device) /* ??? (implicit blockSize: Int) */ = {
  //     val that = blockReduce(f)
  //     that.reduceLeft(f)
  //   }
  //   def blockReduce(f: (A,A) => A)(implicit dev: Device) /* ??? (implicit blockSize: Int) */ = {
  //     val k = /* memoize */ dev.compile( (a:BBArray[A], b:BBArray[A]) => GPUArray.blockReduce(a, b, f) )
  //     val that = BBArray.ofDim[B](a.length / blockSize)
  //     k(this, that)
  //     new GPUArray(that)
  //   }
  // }
  //
  // kinda want this:
  // trait Kernel1[A,B] extends Function1[A,B]
  // val k = mapk(_*2) compose reducek(_+_)
  // k(a, b)
  //
  // val a = BBArray.tabulate[Float](1000000)(_.toFloat)
  // val g = GPUArray(a, dev)
  // val b = g.map(_*2).reduce(_+_)
  //
  // g.map returns a MapKernel1
  //
  //
  // val k1 = dev.compile( ... GPUArray.map(.., _*2) )
  // val k2 = dev.compile( ... GPUArray.blockReduce(.., _+_) )
  // 

/*
  @Deprecated
  def f2bbarrayMapk1[A,B](f: A => B)(implicit ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], dev: Device): BBArrayMapKernel1[A,B] = {
    val kernelName = freshName("theKernel")
    val src = compileMapKernel1(f, kernelName)
    println(src)
    implicit val Ma = ma.manifest
    implicit val Mb = mb.manifest
    implicit val ama = implicitly[BBArrayMarshal[A]]
    implicit val amb = implicitly[BBArrayMarshal[B]]
    val kernel = dev.compile1[BBArray[A], BBArray[B]](kernelName, src,
                                                         new SimpleArrayDist1[BBArray[A]],
                                                         new SimpleGlobalArrayEffect1[B,BBArray[A]])
    new BBArrayMapKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }

  @Deprecated
  def f2bbarrayMapk2[A1,A2,B](f: (A1,A2) => B)(implicit ma1: FixedSizeMarshal[A1], ma2: FixedSizeMarshal[A2], mb: FixedSizeMarshal[B], dev: Device): BBArrayMapKernel2[A1,A2,B] = {
    val kernelName = freshName("theKernel")
    val src = compileMapKernel2(f, kernelName)
    println(src)
    implicit val Ma1 = ma1.manifest
    implicit val Ma2 = ma2.manifest
    implicit val Mb = mb.manifest
    implicit val ama1 = implicitly[BBArrayMarshal[A1]]
    implicit val ama2 = implicitly[BBArrayMarshal[A2]]
    implicit val amb = implicitly[BBArrayMarshal[B]]
    val kernel = dev.compile2[BBArray[A1], BBArray[A2], BBArray[B]](kernelName, src,
                                                         new SimpleArrayDist2[BBArray[A1], BBArray[A2]],
                                                         new SimpleGlobalArrayEffect2[B,BBArray[A1],BBArray[A2]])
    new BBArrayMapKernel2[A1,A2,B] {
      def apply(a1: BBArray[A1], a2: BBArray[A2]) = kernel(a1, a2)
    }
  }

  @Deprecated
  def f2bbarrayReducek1[A](f: (A,A) => A)(implicit ma: FixedSizeMarshal[A], dev: Device): BBArrayReduceKernel1[A] = {
    val kernelName = freshName("theKernel")
    val src = compileReduceKernel1(f, kernelName)
    println(src)
    implicit val ama = implicitly[BBArrayMarshal[A]]
    val numThreads = 128 // dev.device.localMemSize.toInt / 4
    println("numThreads = " + numThreads)
    val d = new BlockArrayDist1[BBArray[A]](numThreads)
    val e = new SimpleLocalArrayWithOutputEffect1[A,BBArray[A]](numThreads, numThreads * fixedSizeMarshal[A].size)
    val kernel = dev.compile1[BBArray[A], BBArray[A]](kernelName, src, d, e)

    new BBArrayReduceKernel1[A] {
      def apply(a: BBArray[A]) = new Future[A] {

        println(d(a))
        println(e(a))

        lazy val future: Future[BBArray[A]] = kernel(a).start
        def run: Unit = future

        def finish: A = {
          val result = future.force
          println("reduce result = " + result)
          result.reduceLeft(f)
        }
      }
    }
  }

  @Deprecated
  def f2bbarrayLocalReducek1[A,B,L](f: BBArray[A] => GroupIndexed1[B] => (Id1, LocalIndexed1[L]) => Unit)(implicit ml: FixedSizeMarshal[L], ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], dev: Device): BBArrayLocalReduceKernel1[A,B] = {
    val kernelName = freshName("theKernel")
    val src = compileReduceKernel1(f, kernelName)
    println(src)
    implicit val ama = implicitly[BBArrayMarshal[A]]
    val numThreads = 128 // dev.device.localMemSize.toInt / 4
    println("numThreads = " + numThreads)
    val d = new BlockArrayDist1[BBArray[A]](numThreads)
    // size of the local buffer is numThreads * sizeof(L)
    val e = new SimpleLocalArrayWithOutputEffect1[B,BBArray[A]](numThreads, numThreads * fixedSizeMarshal[L].size)
    val kernel = dev.compile1[BBArray[A], BBArray[B]](kernelName, src, d, e)
    new BBArrayLocalReduceKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }
  */
}

object Compose {
/*
  (x,y).zipWith(f).reduce(g)
  =>
  Arg2(x,y).zipWith(f).reduce(g) : Future[B]

  k = zipWith(f).reduce(g): Arg => Future[B]
*/

  val varNames = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def genVarNames(xs: List[_]): List[String] = {
    // TODO: what happens when i >= 52 ?
    xs.indices.toList.map(i => varNames(i).toString)
  }

  // TODO: move to Tree
  object Prototype {
    def apply(typ: Tree, name: Id, formals: List[Tree]): Prototype = Prototype(typ, name.name, formals)
  }
  case class Prototype(typ: Tree, name: String, formals: List[Tree]) extends Tree {
    def toCL = typ.toCL + " " + name + formals.map((t:Tree) => t.toCL).mkString("(", ", ", ");\n\n")
  }

  def compileToTreeName(src: AnyRef, arity: Int): (String,List[Tree]) = {
    val k = src.getClass
    val apply = Compiler.findApplyMethod(src, arity)
    val trees = compileRoot(k.getName, Compiler.signature(apply)).reverse
    (methodName(apply), trees)
  }
  
  def compileToTree(src: AnyRef, arity: Int): (Tree,List[Tree]) = {
    val k = src.getClass
    val apply = Compiler.findApplyMethod(src, arity)
    val trees = compileRoot(k.getName, Compiler.signature(apply)).reverse
    (Call(Id(methodName(apply)), (0 until arity).map(i => Id(varNames(i).toString)).toList), trees)
  }
  
  trait KernelLike {
    def trees: List[Tree]

    lazy val src = header + structs + prototypes + functions + kernelSrc(trees)

    private def header: String = ("\n" +
      "typedef char jbyte;                                  \n" +
      "typedef short jshort;                                \n" +
      "typedef ushort jchar;                                \n" +
      "typedef int jint;                                    \n" +
      "typedef long jlong;                                  \n" +
      "typedef float jfloat;                                \n" +
      "typedef double jdouble;                              \n" +
      "typedef char jboolean;                               \n" +
      "typedef union {                                      \n" +
      "  jbyte b;                                           \n" +
      "  jshort s;                                          \n" +
      "  jchar c;                                           \n" +
      "  jint i;                                            \n" +
      "  jlong l;                                           \n" +
      "  jfloat f;                                          \n" +
      "  jdouble d;                                         \n" +
      "  __global void *gp;                                 \n" +
      "  __local void *lp;                                  \n" +
      "} __any__;                                           \n" +
      "struct Tuple2 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "};                                                   \n" +
      "struct Tuple3 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "};                                                   \n" +
      "struct Tuple4 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "};                                                   \n" +
      "struct Tuple5 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "  __any__ _5;                                        \n" +
      "};                                                   \n" +
      "struct Tuple6 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "  __any__ _5;                                        \n" +
      "  __any__ _6;                                        \n" +
      "};                                                   \n" +
      "\n")

    private def structs = trees.map {
        case t @ StructDef(name, fields) => t.toCL + "\n"
        case t => ""
      }.mkString("")

    private def prototypes = trees.map {
        case t @ FunDef(returnType, name, formals, _) => Prototype(returnType, name, formals).toCL + "\n"
        case t => ""
      }.mkString("")

    private def functions = trees.map {
        case t @ FunDef(_, _, _, _) => t.toCL + "\n\n"
        case t => ""
      }.mkString("")

    protected def kernelSrc(trees: List[Tree]): String
  }
}
