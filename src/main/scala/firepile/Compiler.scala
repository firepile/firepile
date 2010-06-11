package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.Spaces._
import firepile.tree.Trees._

import compiler.JVM2CL.compileRoot
import compiler.JVM2CL.mangleName
import compiler.JVM2CL.methodName

import firepile.Implicits._
import java.nio.ByteBuffer

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

object Compose {
  class ArgHasLength[A,X<:Arg[A,X]] extends HasLength[X] {
    def length(a: X) = a.length
  }
  implicit def ahl[A,X<:Arg[A,X]] = new ArgHasLength[A,X]

  implicit def arg2array[A](a: Arg[A,_]) = a.value

  class Arg1Marshal[A:FixedSizeMarshal] extends Marshal[Arg1[A]] {
    def sizes(a: Arg1[A]) = sizes(a.length)
    def sizes(len: Int) = (len * implicitly[FixedSizeMarshal[A]].size) :: Nil
    def align: Int = implicitly[FixedSizeMarshal[A]].align
    def toBuffer(a: Arg1[A]) = a.buffers
    def fromBuffer(bs: List[ByteBuffer]) = bs match {
      case b :: Nil => new Arg1(new BBArray[A](b))
      case _ => throw new MatchError
    }
  }

  class Arg2Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal] extends Marshal[Arg2[A1,A2]] {
    def sizes(a: Arg2[A1,A2]) = sizes(a.length)
    def sizes(len: Int) = 
        (len * implicitly[FixedSizeMarshal[A1]].size) ::
        (len * implicitly[FixedSizeMarshal[A2]].size) :: Nil
    def align: Int = implicitly[FixedSizeMarshal[A1]].align max implicitly[FixedSizeMarshal[A2]].align
    def toBuffer(a: Arg2[A1,A2]) = a.buffers
    def fromBuffer(bs: List[ByteBuffer]) = bs match {
      case b1 :: b2 :: Nil => new Arg2(new BBArray[A1](b1), new BBArray[A2](b2))
      case _ => throw new MatchError
    }
  }

  implicit def Arg1Marshal[A:FixedSizeMarshal] = new Arg1Marshal[A]
  implicit def Arg2Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal] = new Arg2Marshal[A1,A2]

  sealed abstract class Arg[A: FixedSizeMarshal,ArgA<:Arg[A,ArgA]] {

    this : ArgA =>

    def mapk[B,ArgB<:Arg[B,ArgB]](m: Mapper[A,B,ArgA,ArgB])(implicit dev: Device, mb: FixedSizeMarshal[B]) = new MapKernel[A,B,ArgA,ArgB](dev, this, m.trees, m.mapTree, m.builder, m.mab)
    def length: Int
    def arity: Int
    def buffers: List[ByteBuffer]
    def reduce(f: (A,A) => A) = value.reduceLeft(f)
    def value: BBArray[A]
  }

  def idMapper[A:FixedSizeMarshal]: Mapper[A,A,Arg1[A],Arg1[A]] = f2Mapper[A,A]((x:A) => x)

  class Arg1[A:FixedSizeMarshal](a: BBArray[A]) extends Arg[A,Arg1[A]] {
    val m = idMapper[A]
    def reduceBlock(r: Reducer[A])(implicit dev: Device) = new MapBlockReduceKernel[A,A,Arg1[A],Arg1[A]](dev, this, m.trees, r.trees, m.mapTree, r.reduceTree, m.builder, m.mab)
    def reduce(r: Reducer[A])(implicit dev: Device) = reduceBlock(r)(dev).reduce(r.reduceFun)
    def length = a.length
    def arity = 1
    def buffers = a.buffer :: Nil
    def value = a
  }
  class Arg2[A1:FixedSizeMarshal, A2:FixedSizeMarshal](a1: BBArray[A1], a2: BBArray[A2]) extends Arg[(A1,A2),Arg2[A1,A2]] {
    assert(a1.length == a2.length)
    def length = a1.length
    def arity = 2
    def zipWith[B](m: Mapper[(A1,A2),B,Arg2[A1,A2],Arg1[B]])(implicit dev: Device, mb: FixedSizeMarshal[B]) = mapk(m)(dev, mb)
    def buffers = a1.buffer :: a2.buffer :: Nil
    def value = a1 zip a2
  }
  class Arg3[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal](a1: BBArray[A1], a2: BBArray[A2], a3: BBArray[A3]) extends Arg[(A1,A2,A3),Arg3[A1,A2,A3]] {
    assert(a1.length == a2.length)
    assert(a1.length == a3.length)
    def length = a1.length
    def arity = 3
    def zipWith[B](m: Mapper[(A1,A2,A3),B,Arg3[A1,A2,A3],Arg1[B]])(implicit dev: Device, mb: FixedSizeMarshal[B]) = mapk(m)(dev, mb)
    def buffers = a1.buffer :: a2.buffer :: a3.buffer :: Nil
    def value = BBArray.fromArray((0 until a1.length).map(i => (a1(i),a2(i),a3(i))).toArray)
  }

  implicit def Arg1[A:FixedSizeMarshal](a: BBArray[A]) = new Arg1(a)
  implicit def Arg2[A1:FixedSizeMarshal, A2:FixedSizeMarshal](p: (BBArray[A1], BBArray[A2])) = new Arg2(p._1, p._2)
  implicit def Arg3[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal](p: (BBArray[A1], BBArray[A2], BBArray[A3])) = new Arg3(p._1, p._2, p._3)

  def generateMapKernel(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, formals, _) => ("\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      (0 until formals.length).map(i => {
        val Formal(formal, _) = formals(i)
        "   __constant " + formal.toCL + "* " + varNames(i).toString + ",\n" +
        "   const int " + varNames(i).toString + "_len,\n"
      }).mkString("") +
      "   __global " + returnType.toCL + "* output,                                                \n" +
      "   const int output_len)                                                                    \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   output[i] = " + name + "(" +
        (0 until formals.length).map(i => varNames(i).toString + "[i]").mkString(", ") + ");\n" +
      "}                                                                                           \n")
    case _ => throw new RuntimeException("unexpected C AST " + tree.toCL)
  }

  val varNames = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def generateMapReduceKernel(map: Tree, reduce: Tree, kernelName: String): String = (map,reduce) match {
    case (FunDef(mreturnType, mname, mformals, _),
          FunDef(rreturnType, rname, List(Formal(rformal1, _), Formal(rformal2, _)), _))
          if mreturnType.equals(rreturnType) && mreturnType.equals(rformal1) && mreturnType.equals(rformal2) => ("\n" +
      "#define BLOCKSIZE 32                                                                        \n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      (0 until mformals.length).map(i => {
        val Formal(formal, _) = mformals(i)
        "   __constant " + formal.toCL + "* " + varNames(i).toString + ",\n" +
        "   const int " + varNames(i).toString + "_len,\n"
      }).mkString("") +
      "   __global " + rreturnType.toCL + "* output,                                               \n" +
      "   const int output_len,                                                                    \n" +
      "   __local  " + mreturnType.toCL + "* tmp,                                                  \n" +
      "   const int tmp_len)                                                                       \n" +
      "{                                                                                           \n" +
      "   int i = get_global_id(0);                                                                \n" +
      "   int lid = get_local_id(0);                                                               \n" +
      "   tmp[lid] = " + mname + "(" +
        (0 until mformals.length).map(i => varNames(i).toString + "[i]").mkString(", ") + ");\n" +
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
    (Call(Id(methodName(apply)), (0 until arity).map(i => Id(varNames(i).toString)).toList), trees)
  }

  trait KernelLike {
    def trees: List[Tree]

    lazy val src = structs + prototypes + functions + kernelSrc(trees)

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

  case class Mapper[A:FixedSizeMarshal,B:FixedSizeMarshal,ArgA<:Arg[A,ArgA],ArgB<:Arg[B,ArgB]](trees: List[Tree], mapTree: Tree, builder: List[ByteBuffer] => ArgB, mab: Marshal[ArgB])
  case class Reducer[B:FixedSizeMarshal](trees: List[Tree], reduceTree: Tree, reduceFun: (B,B)=>B)

  implicit def f2Mapper[A:FixedSizeMarshal,B:FixedSizeMarshal](f: A=>B): Mapper[A,B,Arg1[A],Arg1[B]] = {
    val (mapTree, trees) = compileToTree(f, 1)
    val builder = (bs: List[ByteBuffer]) => new Arg1[B](new BBArray[B](bs.head))
    val mab = implicitly[Marshal[Arg1[B]]]
    new Mapper[A,B,Arg1[A],Arg1[B]](trees, mapTree, builder, mab)
  }

  implicit def f2Mapper[A1:FixedSizeMarshal,A2:FixedSizeMarshal,B:FixedSizeMarshal](f: (A1,A2)=>B): Mapper[(A1,A2),B,Arg2[A1,A2],Arg1[B]] = {
    val (mapTree, trees) = compileToTree(f, 2)
    val builder = (bs: List[ByteBuffer]) => new Arg1[B](new BBArray[B](bs.head))
    val mab = implicitly[Marshal[Arg1[B]]]
    new Mapper[(A1,A2),B,Arg2[A1,A2],Arg1[B]](trees, mapTree, builder, mab)
  }

  implicit def f2Mapper[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,B:FixedSizeMarshal](f: (A1,A2,A3)=>B): Mapper[(A1,A2,A3),B,Arg3[A1,A2,A3],Arg1[B]] = {
    val (mapTree, trees) = compileToTree(f, 3)
    val builder = (bs: List[ByteBuffer]) => new Arg1[B](new BBArray[B](bs.head))
    val mab = implicitly[Marshal[Arg1[B]]]
    new Mapper[(A1,A2,A3),B,Arg3[A1,A2,A3],Arg1[B]](trees, mapTree, builder, mab)
  }

  implicit def f2Reducer[B:FixedSizeMarshal](f: (B,B)=>B): Reducer[B] = {
    val (reduce, trees) = compileToTree(f, 2)
    Reducer(trees, reduce, f)
  }

  class MapKernel[A:FixedSizeMarshal,B:FixedSizeMarshal,ArgA <: Arg[A,ArgA], ArgB <: Arg[B,ArgB]](dev: Device, input: ArgA, val trees: List[Tree], val mapTree: Tree, builder: List[ByteBuffer] => ArgB, mab: Marshal[ArgB]) extends KernelLike with Future[ArgB] {
    private val kernelName = Compiler.freshName("kernel")

    protected def kernelSrc(trees: List[Tree]): String = {
      val Call(Id(mname), _) = mapTree

      trees.map {
        case t @ FunDef(_, name, formals, _) if (name.equals(mname)) => generateMapKernel(t, kernelName)
        case t => ""
      }.mkString("")
    }

    private lazy val kernel = {
      println(src)
      val d = (a: ArgA) => new Dist {
        def totalNumberOfItems: Int = a.length
      }
      val e = (a: ArgA) => new Effect {
        override def outputSizes = mab.sizes(a.length)
      }
      dev.compile[ArgA,ArgB](kernelName, src, d, e, builder)
    }

    private lazy val future = kernel(input)
    protected def run = future.start
    protected def finish = future.force

    // a.map(this).map(m)
    def map[Z,ArgZ <: Arg[Z,ArgZ]](m: Mapper[B,Z,ArgB,ArgZ])(implicit zm: FixedSizeMarshal[Z], azm: Marshal[ArgZ]): MapKernel[A,Z,ArgA,ArgZ] = new MapKernel[A,Z,ArgA,ArgZ](dev, input, trees ++ m.trees, compose(mapTree, m.mapTree), m.builder, m.mab)
    // a.map(this).reduceBlock(r)
    def reduceBlock(r: Reducer[B]) = new MapBlockReduceKernel[A,B,ArgA,ArgB](dev, input, trees, r.trees, mapTree, r.reduceTree, builder, mab)
    // a.map(this).reduce(r)
    def reduce(r: Reducer[B]) = reduceBlock(r).reduce(r.reduceFun)
  }

  class MapBlockReduceKernel[A:FixedSizeMarshal,B:FixedSizeMarshal,ArgA <: Arg[A,ArgA], ArgB <: Arg[B,ArgB]](dev: Device, input: ArgA, val mapTrees: List[Tree], val reduceTrees: List[Tree], val mapTree: Tree, val reduceTree: Tree, val builder: List[ByteBuffer] => ArgB, val mab: Marshal[ArgB]) extends KernelLike with Future[ArgB] {

    self: MapBlockReduceKernel[A,B,ArgA,ArgB] =>

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
        case (Some(mapFd : FunDef), Some(reduceFd : FunDef)) => generateMapReduceKernel(mapFd, reduceFd, kernelName)
        case _ => throw new RuntimeException("Cannot find either map or reduce function in generated code: map=" + mapFd + " reduce=" + reduceFd)
      }
    }

    def trees = mapTrees ++ reduceTrees

    private lazy val kernel = {
      println(src)
      val numThreads = 128
      val d = (a: ArgA) => new Dist {
        val totalNumberOfItems = (a.length + numThreads - 1) / numThreads
      }
      val e = (a: ArgA) => new Effect {
        // override val outputSizes = a.length * implicitly[FixedSizeMarshal[B]].size
        override val outputSizes = mab.sizes(a.length / numThreads)
        override val localBufferSizes = (numThreads * implicitly[FixedSizeMarshal[B]].size) :: Nil
      }
      dev.compile[ArgA,ArgB](kernelName, src, d, e, builder)
    }

    private lazy val future = kernel(input)
    protected def run = future.start
    protected def finish = future.force

    // a.map(that).blockReduce(this).map(m)
    def map[Z:FixedSizeMarshal,ArgZ<:Arg[Z,ArgZ]](m: Mapper[B,Z,ArgB,ArgZ]) = new ComposeKernel[ArgB,ArgZ](this, (tmp: ArgB) => new MapKernel[B,Z,ArgB,ArgZ](dev, tmp, m.trees, m.mapTree, m.builder, m.mab))

    // a.map(this).blockReduce(this).reduce(r)
    def reduce(r: Reducer[B]): Future[B] = reduce(r.reduceFun)

    def reduce(f: (B,B) => B): Future[B] = new Future[B] {
      def run = self.run

      def finish: B = {
        val z: ArgB = self.future.force
        z.reduce(f)
      }
    }
  }

  private def compose(call1: Tree, call2: Tree) = (call1, call2) match {
    case ( Call(name1, args1 @ List(_)), Call(name2, List(_)) ) => Call(name2, Call(name1, args1))
    case _ => throw new RuntimeException("Cannot compose " + call1 + " and " + call2 + "; not unary functions.")
  }

  class ComposeKernel[A,B](k1: Future[A], k2: A => Future[B]) extends Future[B] {
    protected def run: Unit = k1.start

    // This is slow -- intermediate data gets copied back to the host
    // Should use local memory to copy data.
    protected def finish: B = {
      val tmp: A = k1.force
      val k = k2(tmp)
      k.start
      k.force
    }
  }

  def spawn[B](k: Future[B]) = k.start

  // bitonic sort
  class SortKernel[A]

  class FilterKernel[A,C<:Iterable[A]](input: C) extends Future[C] {
    // TODO
    //
    // Idea: bitonic sort to put elements matching filter before those
    // not matching
    //
    // Idea: sort each block
    //
    protected def run: Unit = {}
    protected def finish: C = input
  }
}
