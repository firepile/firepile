package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.Spaces._
import firepile.tree.Trees._
import firepile.Implicits._
import firepile.Args._

import compiler.JVM2CL.compileRoot
import compiler.JVM2CL.mangleName
import compiler.JVM2CL.methodName

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
}

object Compose {
/*
  (x,y).zipWith(f).reduce(g)
  =>
  Arg2(x,y).zipWith(f).reduce(g) : Future[B]

  k = zipWith(f).reduce(g): Arg => Future[B]
*/

  // usage:
  // val f: (a: BBArray[A]) => Future[B] = dev.zipWith(f).reduce(g)
  class EmptyKernel[A:FixedSizeMarshal,ArgA<:Arg[A,ArgA]](dev: Device) {
    def mapk[B:FixedSizeMarshal,ArgB<:Arg[B,ArgB]](m: Mapper[A,B,ArgA,ArgB]) = new MapKernel[A,B,ArgA,ArgB](dev, m.trees, m.mapTree, m.builder, m.mab)
    def zipWith[B:FixedSizeMarshal,ArgB<:Arg[B,ArgB]](m: Mapper[A,B,ArgA,ArgB]) = new MapKernel[A,B,ArgA,ArgB](dev, m.trees, m.mapTree, m.builder, m.mab)
    def reduceBlock(r: Reducer[A]) = {
      val m = idMapper[A]
      new MapBlockReduceKernel[A,A,Arg1[A],Arg1[A]](dev, m.trees, r.trees, m.mapTree, r.reduceTree, m.builder, m.mab)
    }
    def reduce(r: Reducer[A]) = reduceBlock(r).reduce(r.reduceFun)
  }
  implicit def dev2empty[A:FixedSizeMarshal,ArgA<:Arg[A,ArgA]](dev: Device) = new EmptyKernel[A,ArgA](dev)

  def generateMapKernel(tree: Tree, kernelName: String): String = tree match {
    case FunDef(returnType, name, formals, _) => ("\n" +
      "__kernel void " + kernelName + "(                                                           \n" +
      (0 until formals.length).map(i => {
        val Formal(formal, _) = formals(i)
        "   __global const " + formal.toCL + "* " + varNames(i).toString + ",\n" +
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
        "   __global const " + formal.toCL + "* " + varNames(i).toString + ",\n" +
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
  implicit def f2Mapper[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal,B:FixedSizeMarshal](f: (A1,A2,A3,A4)=>B): Mapper[(A1,A2,A3,A4),B,Arg4[A1,A2,A3,A4],Arg1[B]] = {
    val (mapTree, trees) = compileToTree(f, 3)
    val builder = (bs: List[ByteBuffer]) => new Arg1[B](new BBArray[B](bs.head))
    val mab = implicitly[Marshal[Arg1[B]]]
    new Mapper[(A1,A2,A3,A4),B,Arg4[A1,A2,A3,A4],Arg1[B]](trees, mapTree, builder, mab)
  }
  implicit def f2Mapper[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal,A5:FixedSizeMarshal,B:FixedSizeMarshal](f: (A1,A2,A3,A4,A5)=>B): Mapper[(A1,A2,A3,A4,A5),B,Arg5[A1,A2,A3,A4,A5],Arg1[B]] = {
    val (mapTree, trees) = compileToTree(f, 3)
    val builder = (bs: List[ByteBuffer]) => new Arg1[B](new BBArray[B](bs.head))
    val mab = implicitly[Marshal[Arg1[B]]]
    new Mapper[(A1,A2,A3,A4,A5),B,Arg5[A1,A2,A3,A4,A5],Arg1[B]](trees, mapTree, builder, mab)
  }
  implicit def f2Mapper[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal,A5:FixedSizeMarshal,A6:FixedSizeMarshal,B:FixedSizeMarshal](f: (A1,A2,A3,A4,A5,A6)=>B): Mapper[(A1,A2,A3,A4,A5,A6),B,Arg6[A1,A2,A3,A4,A5,A6],Arg1[B]] = {
    val (mapTree, trees) = compileToTree(f, 3)
    val builder = (bs: List[ByteBuffer]) => new Arg1[B](new BBArray[B](bs.head))
    val mab = implicitly[Marshal[Arg1[B]]]
    new Mapper[(A1,A2,A3,A4,A5,A6),B,Arg6[A1,A2,A3,A4,A5,A6],Arg1[B]](trees, mapTree, builder, mab)
  }

  implicit def f2Reducer[B:FixedSizeMarshal](f: (B,B)=>B): Reducer[B] = {
    val (reduce, trees) = compileToTree(f, 2)
    Reducer(trees, reduce, f)
  }

  class MapKernel[A:FixedSizeMarshal,B:FixedSizeMarshal,ArgA <: Arg[A,ArgA], ArgB <: Arg[B,ArgB]](dev: Device, val trees: List[Tree], val mapTree: Tree, builder: List[ByteBuffer] => ArgB, mab: Marshal[ArgB]) extends KernelLike with Function1[ArgA,Future[ArgB]] {
    private val kernelName = Compiler.freshName("kernel")

    protected def kernelSrc(trees: List[Tree]): String = {
      val Call(Id(mname), _) = mapTree

      trees.map {
        case t @ FunDef(_, name, formals, _) if (name.equals(mname)) => generateMapKernel(t, kernelName)
        case t => ""
      }.mkString("")
    }

    private val kernel = {
      println(src)
      val d = (a: ArgA) => new Dist {
        def totalNumberOfItems: Int = a.length
      }
      val e = (a: ArgA) => new Effect {
        override def outputSizes = mab.sizes(a.length)
      }
      dev.compile[ArgA,ArgB](kernelName, src, d, e, builder)
    }

    def apply(input: ArgA) = new Future[ArgB] {
      private lazy val future = kernel(input)
      protected def run: Unit = future.start
      protected def finish = future.force
    }

    // a.map(this).map(m)
    def map[Z,ArgZ <: Arg[Z,ArgZ]](m: Mapper[B,Z,ArgB,ArgZ])(implicit zm: FixedSizeMarshal[Z], azm: Marshal[ArgZ]): MapKernel[A,Z,ArgA,ArgZ] = new MapKernel[A,Z,ArgA,ArgZ](dev, trees ++ m.trees, composeTrees(mapTree, m.mapTree), m.builder, m.mab)
    // a.map(this).reduceBlock(r)
    def reduceBlock(r: Reducer[B]) = new MapBlockReduceKernel[A,B,ArgA,ArgB](dev, trees, r.trees, mapTree, r.reduceTree, builder, mab)
    // a.map(this).reduce(r)
    def reduce(r: Reducer[B]) = reduceBlock(r).reduce(r.reduceFun)
  }

  class MapBlockReduceKernel[A:FixedSizeMarshal,B:FixedSizeMarshal,ArgA <: Arg[A,ArgA], ArgB <: Arg[B,ArgB]](dev: Device, val mapTrees: List[Tree], val reduceTrees: List[Tree], val mapTree: Tree, val reduceTree: Tree, val builder: List[ByteBuffer] => ArgB, val mab: Marshal[ArgB]) extends KernelLike with Function1[ArgA,Future[ArgB]] {

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

      (mapFd, reduceFd) match {
        case (Some(mapFd : FunDef), Some(reduceFd : FunDef)) => generateMapReduceKernel(mapFd, reduceFd, kernelName)
        case _ => throw new RuntimeException("Cannot find either map or reduce function in generated code: map=" + mapFd + " reduce=" + reduceFd)
      }
    }

    def trees = mapTrees ++ reduceTrees

    private val kernel = {
      println(src)
      val d = (a: ArgA) => new Dist {
        val numThreads = 128
        val totalNumberOfItems = (a.length + numThreads - 1) / numThreads
        override val numberOfItemsPerGroup = numThreads
        println("block reduce: " + a.length + " -> " + totalNumberOfItems)
      }
      val e = (a: ArgA) => new Effect {
        val numThreads = 128
        // override val outputSizes = a.length * implicitly[FixedSizeMarshal[B]].size
        override val outputSizes = mab.sizes((a.length + numThreads - 1) / numThreads)
        override val localBufferSizes = (numThreads * implicitly[FixedSizeMarshal[B]].size) :: Nil
        println("block reduce: " + a.length + " -> " + outputSizes)
      }
      dev.compile[ArgA,ArgB](kernelName, src, d, e, builder)
    }

    def apply(input: ArgA) = new Future[ArgB] {
      private lazy val future = kernel(input)
      protected def run: Unit = future.start
      protected def finish = future.force
    }

    // a.map(that).blockReduce(this).map(m)
    def map[Z:FixedSizeMarshal,ArgZ<:Arg[Z,ArgZ]](m: Mapper[B,Z,ArgB,ArgZ]) = new ComposeKernel[ArgA,ArgB,ArgZ](this, new MapKernel[B,Z,ArgB,ArgZ](dev, m.trees, m.mapTree, m.builder, m.mab))

    // a.map(this).blockReduce(this).reduce(r)
    def reduce(r: Reducer[B]): ArgA => Future[B] = reduce(r.reduceFun)

    def reduce(f: (B,B) => B): ArgA => Future[B] = (a:ArgA) => new Future[B] {
      private lazy val future: Future[ArgB] = self(a)
      protected def run: Unit = future.start
      protected def finish: B = {
        val z: ArgB = future.force
        z.reduce(f)
      }
    }
  }

  private def composeTrees(call1: Tree, call2: Tree) = (call1, call2) match {
    case ( Call(name1, args1 @ List(_)), Call(name2, List(_)) ) => Call(name2, Call(name1, args1))
    case _ => throw new RuntimeException("Cannot compose " + call1 + " and " + call2 + "; not unary functions.")
  }

  class ComposeKernel[X,A,B](k1: Function1[X,Future[A]], k2: A => Future[B]) extends Function1[X,Future[B]] {
    def apply(x: X) = new Future[B] {
      private lazy val future = k1(x)
      protected def run: Unit = future.start

      // This is slow -- intermediate data gets copied back to the host
      // Should use local memory to copy data.
      protected def finish: B = {
        val tmp: A = future.force
        val k = k2(tmp)
        k.start
        k.force
      }
    }
  }

  def spawn[B](k: Future[B]) = k.start
  def spawn[A,B](k: Function1[A,Future[B]]) = (a:A) => k(a).start
}
