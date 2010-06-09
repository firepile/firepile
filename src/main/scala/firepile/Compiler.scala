package firepile

import firepile.util.BufferBackedArray._
import firepile.Spaces._
import firepile.tree.Trees._

import compiler.JVM2CL.compileRoot
import compiler.JVM2CL.mangleName
import compiler.JVM2CL.methodName

import firepile.Implicits._

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
    "#define ARRAY_TYPE(Q,T) T ## Q ## _array;                                                     \n" +
    "#define ARRAY_DECL(Q,T) typedef struct { const int len; __ ## Q T *data; } ARRAY_TYPE(Q,T);   \n" +
    "                                                                                              \n" +
    "ARRAY_DECL(constant, char)                                                                    \n" +
    "ARRAY_DECL(constant, short)                                                                   \n" +
    "ARRAY_DECL(constant, ushort)                                                                  \n" +
    "ARRAY_DECL(constant, int)                                                                     \n" +
    "ARRAY_DECL(constant, long)                                                                    \n" +
    "ARRAY_DECL(constant, float)                                                                   \n" +
    "ARRAY_DECL(constant, double)                                                                  \n" +
    "                                                                                              \n" +
    "ARRAY_DECL(global, char)                                                                      \n" +
    "ARRAY_DECL(global, short)                                                                     \n" +
    "ARRAY_DECL(global, ushort)                                                                    \n" +
    "ARRAY_DECL(global, int)                                                                       \n" +
    "ARRAY_DECL(global, long)                                                                      \n" +
    "ARRAY_DECL(global, float)                                                                     \n" +
    "ARRAY_DECL(global, double)                                                                    \n" +
    "                                                                                              \n" +
    "ARRAY_DECL(local,  char)                                                                      \n" +
    "ARRAY_DECL(local,  short)                                                                     \n" +
    "ARRAY_DECL(local,  ushort)                                                                    \n" +
    "ARRAY_DECL(local,  int)                                                                       \n" +
    "ARRAY_DECL(local,  long)                                                                      \n" +
    "ARRAY_DECL(local,  float)                                                                     \n" +
    "ARRAY_DECL(local,  double)                                                                    \n" +
    "                                                                                              \n" +
    "ARRAY_DECL(private, char)                                                                     \n" +
    "ARRAY_DECL(private, short)                                                                    \n" +
    "ARRAY_DECL(private, ushort)                                                                   \n" +
    "ARRAY_DECL(private, int)                                                                      \n" +
    "ARRAY_DECL(private, long)                                                                     \n" +
    "ARRAY_DECL(private, float)                                                                    \n" +
    "ARRAY_DECL(private, double)                                                                   \n" +
    "                                                                                              \n" +
    "\n")

  def generateMapKernel1(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, List(Formal(formal, _)), _) => ("\n" +
      "inline " + tree.toCL + "\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "  __constant " + formal.toCL + "* a,                                                        \n" +
      "  const int a_len,                                                                          \n" +
      "  __global " + returnType.toCL + "* output,                                                 \n" +
      "  const int output_len)                                                                     \n" +
      "{                                                                                           \n" +
      "  int i = get_global_id(0);                                                                 \n" +
      "  // if (i < a_len) /* should not happen by construction -- # work items == array length    \n" +
      "    output[i] = " + name + "(a[i]);                                                         \n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  def generateMapKernel2(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, List(Formal(formal1, _), Formal(formal2, _)), _) => ("\n" +
      "inline " + tree.toCL + "\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "   __constant     " + formal1.toCL + "* a,                                                  \n" +
      "   const int a_len,                                                                         \n" +
      "   __constant     " + formal2.toCL + "* b,                                                  \n" +
      "   const int b_len,                                                                         \n" +
      "   __global " + returnType.toCL + "* output,                                                \n" +
      "   const int output_len)                                                                    \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   output[i] = " + name + "(a[i], b[i]);                                                    \n" +
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
    println(funs)

    if (funs.isEmpty)
      throw new RuntimeException("Could not compile method in " + k.getName + "." + apply.getName)

    funs.map {
      case fd @ FunDef(returnType, name, List(_), _) if (name.equals(methodName(apply))) => generateMapKernel1(fd, kernelName)
      case fd => fd.toCL + "\n"
    }.mkString("\n")
  }

  /*
  def compileMapKernel1(src: Function1[_,_], name: String): String =
    generateMapKernel1(FunDef(ValueType("float"), Id("apply"), List(Formal(ValueType("float"), Id("x"))), Return(Bin(Id("x"), "*", FloatLit(2.f)))), name)
    */

  def compileMapKernel2(src: Function2[_,_,_], kernelName: String): String = {
    val k = src.getClass
    val apply = findApplyMethod(src, 2)
    val funs = compileRoot(k.getName, signature(apply)).reverse
    println(funs)

    if (funs.isEmpty)
      throw new RuntimeException("Could not compile method in " + k.getName + "." + apply.getName)

    funs.map {
      case fd @ FunDef(returnType, name, List(_, _), _) if (name.equals(methodName(apply))) => generateMapKernel2(fd, kernelName)
      case fd => fd.toCL + "\n"
    }.mkString("\n")
  }
    /*
    generateMapKernel2(FunDef(ValueType("float"), Id("apply"), List(Formal(ValueType("float"), Id("a")),
                                                                    Formal(ValueType("float"), Id("b"))),
                                                              Return(Bin(Bin(Id("a"), "*", Call(Id("sin"), Id("b"))), "+", FloatLit(1.f)))), kernelName)
                                                              */
    /*
    generateMapKernel2(FunDef(ValueType("float"), Id("apply"), List(Formal(ValueType("float"), Id("x")),
                                                                    Formal(ValueType("float"), Id("y"))),
                                                              Return(Bin(Id("x"), "+", Id("y")))), kernelName)
                                                              */

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
"  __constant     T *g_idata, /* thread indexed */                                  \n" +
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

  def f2bbarrayLocalReducek1[A,B,L](f: BBArray[A] => BlockIndexed1[B] => (Id1, LocalThreadIndexed1[L]) => Unit)(implicit ml: FixedSizeMarshal[L], ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], dev: Device): BBArrayLocalReduceKernel1[A,B] = {
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
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// New version of compiler.  Can't get it to compile in its own file.  Frustrating as fuck.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
object Compose {
  class Xyzzy1[A:FixedSizeMarshal](a: BBArray[A]) {
    def mapk[B](m: Mapper1[A,B])(implicit dev: Device, mb: FixedSizeMarshal[B]) = new MapKernel[A,B,BBArray](dev, a, m.trees, m.mapTree)
  }
  implicit def xyzzy1[A:FixedSizeMarshal](a: BBArray[A]) = new Xyzzy1[A](a)
  class Xyzzy2[A1:FixedSizeMarshal, A2:FixedSizeMarshal](a1: BBArray[A1], a2:BBArray[A2]) {
    def mapk[B](m: Mapper2[A1,A2,B])(implicit dev: Device, mb: FixedSizeMarshal[B]) = new MapKernel[(A1,A2),B,BBArray](dev, (a1 zip a2), m.trees, m.mapTree)
  }
  implicit def xyzzy2[A1:FixedSizeMarshal,A2:FixedSizeMarshal](p: (BBArray[A1],BBArray[A2])) = new Xyzzy2[A1,A2](p._1, p._2)

  def generateMapKernel1(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, List(Formal(formal, _)), _) => ("\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "   __constant " + formal.toCL + "* a,                                                       \n" +
      "   const int a_len,                                                                         \n" +
      "   __global " + returnType.toCL + "* output,                                                \n" +
      "   const int output_len)                                                                    \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   output[i] = " + name + "(a[i]);                                                          \n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  def generateZipWithKernel2(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, List(Formal(formal1, _), Formal(formal2, _)), _) => ("\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "   __constant " + formal1.toCL + "* a,                                                      \n" +
      "   const int a_len,                                                                         \n" +
      "   __constant " + formal2.toCL + "* b,                                                      \n" +
      "   const int b_len,                                                                         \n" +
      "   __global " + returnType.toCL + "* output,                                                \n" +
      "   const int output_len)                                                                    \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   output[i] = " + name + "(a[i], b[i]);                                                    \n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  def generateZipWithKernel3(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, List(Formal(formal1, _), Formal(formal2, _), Formal(formal3, _)), _) => ("\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "   __constant " + formal1.toCL + "* a,                                                      \n" +
      "   const int a_len,                                                                         \n" +
      "   __constant " + formal2.toCL + "* b,                                                      \n" +
      "   const int b_len,                                                                         \n" +
      "   __constant " + formal3.toCL + "* c,                                                      \n" +
      "   const int c_len,                                                                         \n" +
      "   __global " + returnType.toCL + "* output,                                                \n" +
      "   const int output_len)                                                                    \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   output[i] = " + name + "(a[i], b[i], c[i]);                                              \n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  def generateMapReduceKernel1(map: Tree, reduce: Tree, kernelName: String): String = (map,reduce) match {
    case (FunDef(mreturnType, mname, List(Formal(mformal1, _)), _),
          FunDef(rreturnType, rname, List(Formal(rformal1, _), Formal(rformal2, _)), _))
          if mreturnType.equals(rreturnType) && mreturnType.equals(rformal1) && mreturnType.equals(rformal2) => ("\n" +
      "#define BLOCKSIZE 32                                                                        \n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "   __constant " + mformal1.toCL + "* a,                                                     \n" +
      "   const int a_len,                                                                         \n" +
      "   __global " + rreturnType.toCL + "* output,                                               \n" +
      "   const int output_len,                                                                    \n" +
      "   __local  " + mreturnType.toCL + "* tmp,                                                  \n" +
      "   const int tmp_len)                                                                       \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   int lid = get_local_id(0);                                                               \n" +
      "   tmp[lid] = " + mname + "(a[i]);                                                          \n" +
      "   barrier(CLK_LOCAL_MEM_FENCE);                                                            \n" +
      "                                                                                            \n" +
      "   int n = get_local_size(0);                                                               \n" +
      "   int s = n/2;                                                                             \n" +
      "   while (s >= BLOCKSIZE) {                                                                 \n" +
      "     tmp[lid] = " + rname + "(tmp[lid], tmp[lid+s]);                                        \n" +
      "     barrier(CLK_LOCAL_MEM_FENCE);                                                          \n" +
      "     s /= 2;                                                                                \n" +
      "   }                                                                                        \n" +
      "                                                                                            \n" +
      "   // Unroll the last loop and don't use barriers since                                     \n" +
      "   // all accesses are within a block.                                                      \n" +
      "   if (BLOCKSIZE >= 64) tmp[lid] = tmp[lid+32];                                             \n" +
      "   if (BLOCKSIZE >= 32) tmp[lid] = tmp[lid+16];                                             \n" +
      "   if (BLOCKSIZE >= 16) tmp[lid] = tmp[lid+8];                                              \n" +
      "   if (BLOCKSIZE >=  8) tmp[lid] = tmp[lid+4];                                              \n" +
      "   if (BLOCKSIZE >=  4) tmp[lid] = tmp[lid+2];                                              \n" +
      "   if (BLOCKSIZE >=  2) tmp[lid] = tmp[lid+1];                                              \n" +
      "                                                                                            \n" +
      "   if (lid == 0) {                                                                          \n" +
      "     int bid = get_group_id(0);                                                             \n" +
      "     output[bid] = tmp[0];                                                                  \n" +
      "   }                                                                                        \n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + map.toCL + " or " + reduce.toCL)
  }

  def generateReduceKernel1(tree: Tree, kernelName: String): String = tree match {
    case (FunDef(rreturnType, rname, List(Formal(rformal1, _), Formal(rformal2, _)), _))
       if rreturnType.equals(rformal1) && rreturnType.equals(rformal2) => ("\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      "   __constant " + rformal1.toCL + "* a,                                                     \n" +
      "   const int a_len,                                                                         \n" +
      "   __global " + rreturnType.toCL + "* output,                                               \n" +
      "   const int output_len,                                                                    \n" +
      "   __local  " + rreturnType.toCL + "* tmp,                                                  \n" +
      "   const int tmp_len)                                                                       \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   int lid = get_local_id(0);                                                               \n" +
      "   tmp[lid] = a[i];                                                                         \n" +
      "   barrier(CLK_LOCAL_MEM_FENCE);                                                            \n" +
      "   // Do the reduction.  This is O(n)--we could make it O(log n) with some effort.          \n" +
      "   if (lid == 0) {                                                                          \n" +
      "     int n = get_local_size(0);                                                             \n" +
      "     int bid = get_group_id(0);                                                             \n" +
      "     " + rreturnType.toCL + " t = tmp[lid];                                                 \n" +
      "     for (int j = lid+1; j < lid+n; j++) {                                                  \n" +
      "       t = " + rname + "(t, tmp[j]);                                                        \n" +
      "     }                                                                                      \n" +
      "     output[bid] = t;                                                                       \n" +
      "   }                                                                                        \n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  object Prototype {
      def apply(typ: Tree, name: Id, formals: List[Tree]): Prototype = Prototype(typ, name.name, formals)
  }
  case class Prototype(typ: Tree, name: String, formals: List[Tree]) extends Tree {
      def toCL = typ.toCL + " " + name + formals.map((t:Tree) => t.toCL).mkString("(", ", ", ");\n\n")
  }

  private def compileToTree(src: AnyRef, arity: Int): (Tree,List[Tree]) = {
    val k = src.getClass
    val apply = Compiler.findApplyMethod(src, arity)
    val trees = compileRoot(k.getName, Compiler.signature(apply)).reverse
    val vars = "abcdefg";
    (Call(Id(methodName(apply)), (0 until arity).map(i => Id(vars(i).toString)).toList), trees)
  }

  def compileMap1[A,B](f: A => B)(implicit ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], dev: Device): BBArray[A] => MapKernel[A,B,BBArray] = {
    val (call, trees) = compileToTree(f, 1)
    (a: BBArray[A]) => new MapKernel[A,B,BBArray](dev, a, trees, call)
  }

//  def compileZipWith2[A1,A2,B](f: (A1,A2) => B)(implicit ma1: FixedSizeMarshal[A1], ma2: FixedSizeMarshal[A2], mb: FixedSizeMarshal[B], dev: Device): (BBArray[A1], BBArray[A2]) => MapKernel2[A1,A2,B] = {
//    val (call, trees) = compileToTree(f, 2)
//    (a1: BBArray[A1], a2: BBArray[A2]) => new MapKernel2[A1,A2,B](dev, a1, a2, trees, call)
//  }

//  def compileZipWith3[A1,A2,A3,B](f: (A1,A2,A3) => B)(implicit ma1: FixedSizeMarshal[A1], ma2: FixedSizeMarshal[A2], ma3: FixedSizeMarshal[A3], mb: FixedSizeMarshal[B], dev: Device): (BBArray[A1], BBArray[A2], BBArray[A3]) => MapKernel3[A1,A2,A3,B] = {
//    val (call, trees) = compileToTree(f, 3)
//    (a1: BBArray[A1], a2: BBArray[A2], a3: BBArray[A3]) => new MapKernel3[A1,A2,A3,B](dev, a1, a2, a3, trees, call)
//  }

  trait KernelLike {
    def trees: List[Tree]

    lazy val src = structs + prototypes + functions + kernelSrc(trees)

    private def structs = trees.map {
        case FunDef(_, _, _, _) => ""
        case t => t.toCL + "\n"
      }.mkString("\n")

    private def prototypes = trees.map {
        case t @ FunDef(returnType, name, formals, _) => Prototype(returnType, name, formals).toCL + "\n"
        case t => ""
      }.mkString("\n")

    private def functions = trees.map {
        case t @ FunDef(_, _, _, _) => t.toCL + "\n"
        case t => ""
      }.mkString("\n")

    protected def kernelSrc(trees: List[Tree]): String
  }

  case class Mapper1[A:FixedSizeMarshal,B:FixedSizeMarshal](trees: List[Tree], mapTree: Tree)
  case class Mapper2[A1:FixedSizeMarshal,A2:FixedSizeMarshal,B:FixedSizeMarshal](trees: List[Tree], mapTree: Tree)
  case class Reducer[B:FixedSizeMarshal](trees: List[Tree], reduceTree: Tree, reduceFun: (B,B)=>B)

  implicit def f2Mapper1[A:FixedSizeMarshal,B:FixedSizeMarshal](f: A=>B) = {
    val (mapTree, trees) = compileToTree(f, 1)
    Mapper1[A,B](trees, mapTree)
  }

  implicit def f2Mapper2[A1:FixedSizeMarshal,A2:FixedSizeMarshal,B:FixedSizeMarshal](f: (A1,A2)=>B) = {
    val (mapTree, trees) = compileToTree(f, 2)
    Mapper2[A1,A2,B](trees, mapTree)
  }

  implicit def f2Reducer[B:FixedSizeMarshal](f: (B,B)=>B) = {
    val (reduce, trees) = compileToTree(f, 2)
    Reducer(trees, reduce, f)
  }

  class MapKernel[A,B,C[X] <: Iterable[X]](dev: Device, input: C[A], val trees: List[Tree], val mapTree: Tree)(implicit ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], mca: Marshal[C[A]], mcb: Marshal[C[B]], hlca: HasLength[C[A]], hlcb: HasLength[C[B]]) extends KernelLike with Future[C[B]] {
    private val kernelName = Compiler.freshName("kernel")

    protected def kernelSrc(trees: List[Tree]): String = {
      val Call(Id(mname), _) = mapTree

      trees.map {
        case t @ FunDef(_, name, List(_), _) if (name.equals(mname)) => generateMapKernel1(t, kernelName)
        case t @ FunDef(_, name, List(_, _), _) if (name.equals(mname)) => generateZipWithKernel2(t, kernelName)
        case t @ FunDef(_, name, List(_, _, _), _) if (name.equals(mname)) => generateZipWithKernel3(t, kernelName)
        case t => ""
      }.mkString("")
    }

    private lazy val kernel = {
      println(src)
      dev.compile1[C[A],C[B]](kernelName, src, new SimpleArrayDist1[C[A]], new SimpleGlobalArrayEffect1[B,C[A]])
    }

    private lazy val future = kernel(input)
    protected def run = future.start
    protected def finish = future.force

    // a.map(this).map(m)
    def map[Z](m: Mapper1[B,Z])(implicit mz: FixedSizeMarshal[Z], mcz: Marshal[C[Z]], hlcz: HasLength[C[Z]]): MapKernel[A,Z,C] = new MapKernel[A,Z,C](dev, input, trees ++ m.trees, compose(mapTree, m.mapTree))
    // a.map(this).reduceBlock(r)
    def reduceBlock(r: Reducer[B]) = new MapBlockReduceKernel[A,B,C](dev, input, trees, r.trees, mapTree, r.reduceTree)
    // a.map(this).reduce(r)
    def reduce(r: Reducer[B]) = reduceBlock(r).reduce(r.reduceFun)
  }

  class MapBlockReduceKernel[A,B,C[X] <: Iterable[X]](dev: Device, input: C[A], val mapTrees: List[Tree], val reduceTrees: List[Tree], val mapTree: Tree, val reduceTree: Tree)(implicit ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], mca: Marshal[C[A]], mcb: Marshal[C[B]], hlca: HasLength[C[A]], hlcb: HasLength[C[B]]) extends KernelLike with Future[C[B]] {

    self: MapBlockReduceKernel[A,B,C] =>

    private val kernelName = Compiler.freshName("kernel")

    protected def kernelSrc(trees: List[Tree]): String = {
      val Call(Id(mname), _) = mapTree
      val Call(Id(rname), _) = reduceTree

      val mapFd = mapTrees.filter {
        case t @ FunDef(_, name, _, _) if name.equals(mname) => true
        case t => false
      }.headOption

      val reduceFd = reduceTrees.filter {
        case t @ FunDef(_, name, List(_, _), _) if (name.equals(rname)) => true
        case t => false
      }.headOption

      println(mapTrees)
      println(mname)
      println(mapFd)

      println(reduceTrees)
      println(rname)
      println(reduceFd)

      (mapFd, reduceFd) match {
        case (Some(mapFd : FunDef), Some(reduceFd : FunDef)) if mapFd.formals.length == 1 => generateMapReduceKernel1(mapFd, reduceFd, kernelName)
        //case (Some(mapFd : FunDef), Some(reduceFd : FunDef)) if mapFd.formals.length == 2 => generateMapReduceKernel2(mapFd, reduceFd, kernelName)
        case _ => throw new RuntimeException("Cannot find either map or reduce function in generated code: map=" + mapFd + " reduce=" + reduceFd)
      }
    }

    def trees = mapTrees ++ reduceTrees

    private lazy val kernel = {
      println(src)
      val numThreads = 128
      val d = new BlockArrayDist1[C[A]](numThreads)
      val e = new SimpleLocalArrayWithOutputEffect1[B,C[A]](numThreads, numThreads * fixedSizeMarshal[A].size)
      dev.compile1[C[A], C[B]](kernelName, src, d, e)
    }

    private lazy val future = kernel(input)
    protected def run = future.start
    protected def finish = future.force

    // a.map(that).blockReduce(this).map(m)
    def map[Z](m: Mapper1[B,Z])(implicit mz: FixedSizeMarshal[Z], mcz: Marshal[C[Z]], hlcz: HasLength[C[Z]]) = new ComposeKernel[B,Z,C](this, (tmp: C[B]) => new MapKernel[B,Z,C](dev, tmp, m.trees, m.mapTree))

    // a.map(this).blockReduce(this).reduce(r)
    def reduce(r: Reducer[B]): Future[B] = reduce(r.reduceFun)

    def reduce(f: (B,B) => B): Future[B] = new Future[B] {
      def run = self.run

      def finish: B = {
        val z: C[B] = self.future.force

        z.reduceLeft(f)
      }
    }
  }

  private def compose(call1: Tree, call2: Tree) = (call1, call2) match {
    case ( Call(name1, args1 @ List(_)), Call(name2, List(_)) ) => Call(name2, Call(name1, args1))
    case _ => throw new RuntimeException("Cannot compose " + call1 + " and " + call2 + "; not unary functions.")
  }

  class ComposeKernel[A,B,C[X] <: Iterable[X]](k1: Future[C[A]], k2: C[A] => Future[C[B]])(implicit ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], mca: Marshal[C[A]], mcb: Marshal[C[B]], hlca: HasLength[C[A]], hlcb: HasLength[C[B]]) extends Future[C[B]] {
    protected def run: Unit = k1.start

    // This is slow -- intermediate data gets copied back to the host
    // Should use local memory to copy data.
    protected def finish: C[B] = {
      val tmp: C[A] = k1.force
      val k = k2(tmp)
      k.start
      k.force
    }
  }

  def spawn[B](k: Future[B]) = k.start

/*
  trait Arg[A:Marshal]
  class GlobalArg[A:Marshal] extends Arg[A]
  class LocalArg[A:Marshal] extends Arg[A]
  class PrivateArg[A:Marshal] extends Arg[A]
  */

  /*
  trait Composition[Kfrom <: KernelLike, F, Kto <: KernelLike] {
    def compose(k: Kfrom, f: F): Kto
  }

  class M1M1[A,B,C] extends Composition[MapKernel1[A,B], Mapper1[B,C], MapKernel1[A,C]] {
    def compose(k: MapKernel1[A,B], f: Mapper[B,C]) = new MapKernel1[A,C](k.dev, k.input, k.trees ++ m.trees, compose(k.mapTree, m.mapTree))
  }

  implicit def M1M1[A,B,C] = new M1M1[A,B,C]
  */

  /*
  class FutureWithMap[A:FixedSizeMarshal, CA[A]: Marshal](k: Future[A]) {
    def mapk[B:FixedSizeMarshal, CB[B]: Marshal](k: Mapper[CA[A], CB[B]]) = new ComposeKernel(this, (tmp: CA[A]) => new MapKernel[CA[A], CB[B]](dev, tmp, m.trees, m.mapTree)
  }
  implicit future2mapper[A:FixedSizeMarshal](k: Future[BBArray[A]]) = new FutureWithMap[BBArray[A]](k)

  implicit x2future[A](a: A): Future[A] = new Future[A] {
    def run: Unit = ()
    def finish = a
  }

  Future[BBArray[A]]
  Future[LocalThreadIndexed[A]].map(Mapper[A,B]): Future[LocalThreadIndexed[B]]
  Future[LocalThreadIndexed[A]].map(Mapper[A,B]): Future[LocalThreadIndexed[B]]
  Future[LocalThreadIndexed[A]].reduce(Reducer[A]): Future[A]

  implicit Future[LocalThreadIndexed[A]] --> Future[BBArray[A]]
*/
}
