package simplecl.tests

import simplecl._
import simplecl.util.Buffer
import simplecl.util.BufferBackedArray._

import java.nio.FloatBuffer
import java.nio.ByteBuffer
import java.nio.{Buffer=>NIOBuffer}
import java.nio.ByteOrder

import scala.reflect.Manifest
import scala.collection.mutable.ArraySeq

import com.nativelibs4java.opencl.CLMem
import com.nativelibs4java.opencl.CLEvent
import com.nativelibs4java.opencl.CLKernel.LocalSize

object OpenCLScalaTest4 {
  object CL {
    private val platforms = SimpleCL.listPlatforms

    private lazy val defaultGPUPlatform: Option[SCLPlatform] = {
      platforms.flatMap {
        p => p.listGPUDevices(true).map {
          d => p
        }
      }.headOption
    }
    private lazy val defaultCPUPlatform: Option[SCLPlatform] = {
      platforms.flatMap {
        p => p.listCPUDevices(true).map {
          d => p
        }
      }.headOption
    }
    private lazy val defaultGPU = {
      defaultGPUPlatform match {
        case Some(p) => p.listGPUDevices(true).headOption
        case None => None
      }
    }

    private lazy val defaultCPU = {
      defaultCPUPlatform match {
        case Some(p) => p.listCPUDevices(true).headOption
        case None => None
      }
    }

    private lazy val bestDevice: SCLDevice = new SCLDevice(platforms(0).bestDevice)

    private lazy val gpuContext: Option[(SCLPlatform, SCLDevice, SCLContext)] = (defaultGPUPlatform, defaultGPU) match {
      case (Some(p), Some(d)) => Some(Triple(p, d, p.createContext(null, d)))
      case _ => None
    }

    lazy val gpu = gpuContext match {
      case Some(Triple(p, d, c)) => new Device(p, d, c)
      case None => null
    }
  }

  class AT[A](marshal: FixedSizeMarshal[A], manifest: ClassManifest[A]) extends Marshal[Array[A]] {
    def size(a: Array[A]) = {
      if (a.length == 0) 0 else marshal.size(a(0)) * a.length
    }
    def align = marshal.align
    override def put(buf:ByteBuffer, i: Int, x: Array[A]) = {
        var j = 0
        var k = i
        while (j < x.length) {
            marshal.put(buf, i+k, x(j))
            j += 1
            k += marshal.size
        }
    }
    override def get(buf:ByteBuffer, i: Int) = {
      implicit val m = marshal // need evidence
      implicit val M = manifest // need evidence
      printBuffer(buf)
      val x = new BBArray[A](buf.position(i).asInstanceOf[ByteBuffer])
      x.toArray
    }
    override def put(a: Array[A]) = {
      implicit val m = marshal // need evidence
      implicit val M = manifest // need evidence
      val x = BBArray.fromArray[A](a)
      x.buffer
    }
    override def get(b: ByteBuffer) = {
      implicit val m = marshal // need evidence
      implicit val M = manifest // need evidence
      printBuffer(b)
      val x = new BBArray[A](b)
      x.toArray
    }
  }

  implicit def AT[A](implicit marshal: FixedSizeMarshal[A], manifest: ClassManifest[A]): AT[A] = new AT[A](marshal, manifest)

  class BBAT[A: FixedSizeMarshal] extends Marshal[BBArray[A]] {
    def size(a: BBArray[A]) = a.length * fixedSizeMarshal[A].size
    def align = fixedSizeMarshal[A].align
    override def put(buf:ByteBuffer, i: Int, x: BBArray[A]) = {
        var j = 0
        var k = i
        while (j < x.length) {
            fixedSizeMarshal[A].put(buf, i+k, x(j))
            j += 1
            k += fixedSizeMarshal[A].size
        }
    }
    override def get(buf:ByteBuffer, i: Int) = {
        new BBArray(buf.position(i).asInstanceOf[ByteBuffer])
    }
    override def put(a: BBArray[A]) = a.buffer
    override def get(b: ByteBuffer) = new BBArray(b)
  }

  implicit def BBAT[A: FixedSizeMarshal]: BBAT[A] = new BBAT[A]

  trait Future[B] { def force: B }

  trait Kernel {
  }
  trait Kernel1[A,B] extends Function1[A,InstantiatedKernel[B]] with Kernel {
  }
  trait Kernel2[A1,A2,B] extends Function2[A1,A2,InstantiatedKernel[B]] with Kernel {
  }
  trait Kernel3[A1,A2,A3,B] extends Function3[A1,A2,A3,InstantiatedKernel[B]] with Kernel {
  }

  trait InstantiatedKernel[B] {
    def run(dev: Device): Future[B]
  }

  class BufKernel1(code: SCLKernel, val dist: Dist, val effect: Effect) extends Kernel1[ByteBuffer,ByteBuffer] {
    def apply(a: ByteBuffer) = new InstantiatedBufKernel(code, dist, effect, a)
  }
  class BufKernel2(code: SCLKernel, val dist: Dist, val effect: Effect) extends Kernel2[ByteBuffer,ByteBuffer,ByteBuffer] {
    def apply(a1: ByteBuffer, a2: ByteBuffer) = new InstantiatedBufKernel(code, dist, effect, a1, a2)
  }
  class BufKernel3(code: SCLKernel, val dist: Dist, val effect: Effect) extends Kernel3[ByteBuffer,ByteBuffer,ByteBuffer,ByteBuffer] {
    def apply(a1: ByteBuffer, a2: ByteBuffer, a3: ByteBuffer) = new InstantiatedBufKernel(code, dist, effect, a1, a2, a3)
  }

  val printBuffers = false

  def printBuffer(a: => ByteBuffer) = if (printBuffers) {
    var bb = a.asFloatBuffer.duplicate
    var i = 0
    while (bb.hasRemaining) {
      println(i + ": " + bb.get)
      i += 1
    }
  }

  class InstantiatedBufKernel(code: SCLKernel, val dist: Dist, val effect: Effect, buffers: ByteBuffer*) extends InstantiatedKernel[ByteBuffer] {
    def run(dev: Device) = {
      val d = dist
      val e = effect
      val memIn = buffers.map(a => dev.global.allocForRead[Byte](a.limit))
      val memOut = dev.global.allocForWrite[Byte](e.outputSize)

      val writeEvents = (buffers zip memIn).map{ case (a,mem) => mem.write(dev.queue, Buffer.fromNIOBuffer[Byte](a), false) }

      val args = memIn.toList ::: memOut :: e.localBufferSizes.map(size => dev.local.allocForReadWrite[Byte](size))
      println(args)
      code.setArgs(args:_*)

      // println(args)

      val runEvent = code.enqueueNDRange(dev.queue, Array(d.totalNumberOfItems), Array(d.numberOfItemsPerGroup), writeEvents:_*)

      readBackResult(dev, e.outputSize, runEvent, memOut)
    }

    def readBackResult(dev: Device, outputLength: Int, runEvent: CLEvent, memOut: SCLBuffer[_]) = {
      new Future[ByteBuffer] {
        def force = {
          val bufOut = ByteBuffer.allocateDirect(outputLength).order(ByteOrder.nativeOrder)
          val readEvent = memOut.read(dev.queue, Buffer.fromNIOBuffer[Byte](bufOut), false, runEvent)
          //dev.queue.enqueueWaitForEvents(readEvent)
          dev.queue.finish
          if (printBuffers) println("len = " + outputLength)
          if (printBuffers) println("output " + bufOut.limit)
          printBuffer(bufOut)
          bufOut.rewind
          bufOut
        }
      }
    }
  }

  def finish[A](dev: Device)(body: => A): A = {
    try {
      body
    }
    finally {
      dev.queue.finish
    }
  }

  trait Effect {
    def outputSize: Int
    def localBufferSizes: List[Int] = Nil
    override def toString = "Effect {out=" + outputSize + "}"
  }

  trait Dist {
    def totalNumberOfItems: Int
    def numberOfItemsPerGroup: Int = 1
    override def toString = "Dist {n=" + totalNumberOfItems + "/" + numberOfItemsPerGroup + "}"
  }

  type Dist1[A] = Function1[A,Dist]
  type Dist2[A1,A2] = Function2[A1,A2,Dist]
  type Dist3[A1,A2,A3] = Function3[A1,A2,A3,Dist]

  type Effect1[A] = Function1[A,Effect]
  type Effect2[A1,A2] = Function2[A1,A2,Effect]
  type Effect3[A1,A2,A3] = Function3[A1,A2,A3,Effect]

  class SimpleArrayDist1[A:FixedSizeMarshal,T <: { def length: Int }] extends Dist1[T] {
    def apply(a: T) = new Dist {
      val totalNumberOfItems = a.length
    }
  }

  class BlockArrayDist1[A:FixedSizeMarshal,T <: { def length: Int }](n: Int = 32) extends Dist1[T] {
    def apply(a: T) = new Dist {
      val totalNumberOfItems = a.length
      override val numberOfItemsPerGroup = n
    }
  }

  class SimpleGlobalArrayEffect1[A:FixedSizeMarshal, T <: { def length: Int }] extends Effect1[T] {
    def apply(a: T) = new Effect {
      val outputSize = a.length * fixedSizeMarshal[A].size
    }
  }

  class SimpleLocalArrayEffect1[A:FixedSizeMarshal, T <: { def length: Int }](n:Int) extends Effect1[T] {
    def apply(a: T) = new Effect {
      val outputSize = a.length * fixedSizeMarshal[A].size
      override val localBufferSizes = List[Int](n)
    }
  }

  class SimpleLocalArrayWithOutputEffect1[A:FixedSizeMarshal, T <: { def length: Int }](numThreads: Int, n:Int) extends Effect1[T] {
    def apply(a: T) = new Effect {
      val outputSize = ((a.length + numThreads - 1) / numThreads) * fixedSizeMarshal[A].size
      override val localBufferSizes = List[Int](n)
    }
  }

  class Device(val platform: SCLPlatform, val device: SCLDevice, val context: SCLContext) {
    private def compileBuffer1(name: String, src: String): (Dist,Effect) => BufKernel1 = {
      val program = context.createProgram(src).build
      val code = program.createKernel(name)
      (d: Dist, e: Effect) => new BufKernel1(code, d, e)
    }

    private def compileBuffer2(name: String, src: String): (Dist,Effect) => BufKernel2 = {
      val program = context.createProgram(src).build
      val code = program.createKernel(name)
      (d: Dist, e: Effect) => new BufKernel2(code, d, e)
    }

    private def compileBuffer3(name: String, src: String): (Dist,Effect) => BufKernel3 = {
      val program = context.createProgram(src).build
      val code = program.createKernel(name)
      (d: Dist, e: Effect) => new BufKernel3(code, d, e)
    }

    def compile1[A: Marshal, B: Marshal](name: String, src: String, dist: Dist1[A], effect: Effect1[A]) = {
      val transA = implicitly[Marshal[A]];
      val transB = implicitly[Marshal[B]];

      val kernel: (Dist,Effect) => BufKernel1 = compileBuffer1(name, src)

      new Kernel1[A,B] {
        def apply(input: A) = new InstantiatedKernel[B] {
          def run(dev: Device) = {
            val bufIn: ByteBuffer = transA.put(input)
            val d: Dist = dist(input)
            val e: Effect = effect(input)
            val k = kernel(d, e)
            val f = k(bufIn).run(dev)

            new Future[B] {
              def force = {
                val bufOut: ByteBuffer = f.force
                val result: B = transB.get(bufOut)
                result
              }
            }
          }
        }
      }
    }

    lazy val queue = context.createDefaultQueue()

    lazy val global = new GlobalMem(this)
    lazy val local = new LocalMem(this)

    def spawn[B](k: InstantiatedKernel[B]) = k.run(this)
  }


    class ArrayKernelWrapper[A](a: Array[A]) {
      def mapKernel[B](k: ArrayMapKernel1[A,B]) = k(a)
      def reduceKernel[B](k: ArrayReduceKernel1[A,B]) = k(a)
    }
    class BBArrayKernelWrapper[A](a: BBArray[A]) {
      def mapKernel[B](k: BBArrayMapKernel1[A,B]) = k(a)
      def reduceKernel[B](k: BBArrayReduceKernel1[A,B]) = k(a)
    }

    implicit def wrapArray[A](a: Array[A]) = new ArrayKernelWrapper[A](a)
    implicit def wrapBBArray[A](a: BBArray[A]) = new BBArrayKernelWrapper[A](a)

    // Kernels that were compiled from functions on single elements
    trait ArrayMapKernel1[A,B] extends Kernel1[Array[A],Array[B]]
    trait ArrayMapKernel2[A1,A2,B] extends Kernel2[Array[A1],Array[A2],Array[B]]
    trait ArrayMapKernel3[A1,A2,A3,B] extends Kernel3[Array[A1],Array[A2],Array[A3],Array[B]]
    trait BBArrayMapKernel1[A,B] extends Kernel1[BBArray[A],BBArray[B]]
    trait BBArrayMapKernel2[A1,A2,B] extends Kernel2[BBArray[A1],BBArray[A2],BBArray[B]]
    trait BBArrayMapKernel3[A1,A2,A3,B] extends Kernel3[BBArray[A1],BBArray[A2],BBArray[A3],BBArray[B]]

    trait ArrayReduceKernel1[A,B] extends Kernel1[Array[A],B]
    trait ArrayReduceKernel2[A1,A2,B] extends Kernel2[Array[A1],Array[A2],B]
    trait ArrayReduceKernel3[A1,A2,A3,B] extends Kernel3[Array[A1],Array[A2],Array[A3],B]
    trait BBArrayReduceKernel1[A,B] extends Kernel1[BBArray[A],B]
    trait BBArrayReduceKernel2[A1,A2,B] extends Kernel2[BBArray[A1],BBArray[A2],B]
    trait BBArrayReduceKernel3[A1,A2,A3,B] extends Kernel3[BBArray[A1],BBArray[A2],BBArray[A3],B]

    class Spawner1[A](a1:Array[A]) {
      def lazyzip[A2](a2:Array[A2]) = new Spawner2(a1,a2)
      def map[B](k: ArrayMapKernel1[A,B]) = k(a1)
    }
    class Spawner2[A1,A2](a1:Array[A1],a2:Array[A2]) {
      def lazyzip[A3](a3:Array[A3]) = new Spawner3(a1,a2,a3)
      def zipMap[B](k: ArrayMapKernel2[A1,A2,B]) = k(a1,a2)
    }
    class Spawner3[A1,A2,A3](a1:Array[A1],a2:Array[A2],a3:Array[A3]) {
      def zipMap[B](k: ArrayMapKernel3[A1,A2,A3,B]) = k(a1,a2,a3)
    }

    implicit def S1[A](a:Array[A]) = new Spawner1[A](a)
    implicit def S2[A1,A2](a:Pair[Array[A1],Array[A2]]) = new Spawner2[A1,A2](a._1,a._2)
    implicit def S3[A1,A2,A3](a:Triple[Array[A1],Array[A2],Array[A3]]) = new Spawner3[A1,A2,A3](a._1,a._2,a._3)

  val floatX2 = (a:Float) => a * 2.0f
  val aSinB = (a:Float,b:Float) => a * Math.sin(b).toFloat + 1.0f

/*
  type Point1 = Int
  case class Point2(val x: Int, val y: Int) extends Tuple2[Int,Int](x, y) {
    def +(i: Int, j: Int)(implicit config: Config2) = Point2((x+i)%config.localThreadIdSpace._1,(y+j)%config.localThreadIdSpace._2)
    def -(i: Int, j: Int)(implicit config: Config2) = Point2((x-i)%config.localThreadIdSpace._1,(y-j)%config.localThreadIdSpace._2)
    def +(p: Point2)(implicit config: Config2) = Point2((x+p.x)%config.localThreadIdSpace._1,(y+p.y)%config.localThreadIdSpace._2)
    def -(p: Point2)(implicit config: Config2) = Point2((x-p.x)%config.localThreadIdSpace._1,(y-p.y)%config.localThreadIdSpace._2)
  }
  case class Point3(val x: Int, val y: Int, val z: Int) extends Tuple3[Int,Int,Int](x, y) {
    def +(i: Int, j: Int, k: Int) = Point3(x+i,y+j,z+k)
    def -(i: Int, j: Int, k: Int) = Point3(x-i,y-j,z-k)
    def +(p: Point3) = Point3(x+p.x,y+p.y,z+p.z)
    def -(p: Point3) = Point3(x-p.x,y-p.y,z-p.z)
  }

  trait Id[IS <: IndexSpace] {
    type Tuple = IS#Tuple
    val indexSpace: IS = config
    val config: IS

    val threadIdSpace = indexSpace.threadIdSpace
    val blockIdSpace = indexSpace.blockIdSpace
    val localThreadIdSpace = indexSpace.localThreadIdSpace

    val global = thread
    def thread: Tuple
    def block: Tuple
    def localThread: Tuple
  }

  class Id1(val config: Config1, val block: Int, val localThread: Int) extends Id[Config1] {
    val thread = block * config.localThreadIdSpace + localThread
  }
  class Id2(val config: Config2, val block: (Int,Int), val localThread: (Int,Int)) extends Id[Config2] {
    val thread = (block._1 * config.localThreadIdSpace._1 + localThread._1,
                  block._2 * config.localThreadIdSpace._2 + localThread._2)
  }
  class Id3(val config: Config3, val block: (Int,Int,Int), val localThread: (Int,Int,Int)) extends Id[Config3] {
    val thread = (block._1 * config.localThreadIdSpace._1 + localThread._1,
                  block._2 * config.localThreadIdSpace._2 + localThread._2,
                  block._3 * config.localThreadIdSpace._3 + localThread._3)
  }

  /** Index space */
  trait IndexSpace {
    type Tuple
    /** Space of global thread ids; must be blockIdSpace * localThreadIdSpace */
    def threadIdSpace: Tuple
    /** Space of block (work group) ids */
    def blockIdSpace: Tuple
    /** Space of local thread (work item) ids witin a block.  All blocks have the same local space. */
    def localThreadIdSpace: Tuple

    def threadIds: Seq[Tuple]
    def blockIds: Seq[Tuple]
    def localThreadIds: Seq[Tuple]
  }
  class Config1(val blockIdSpace: Int, val localThreadIdSpace: Int) extends IndexSpace {
    type Tuple = Int
    def threadIdSpace = blockIdSpace * localThreadIdSpace

    def numThreads = threadIdSpace
    def numBlocks = blockIdSpace
    def numThreadsPerBlock = localThreadIdSpace

    def threadIds: Seq[Tuple] = 0 until threadIdSpace
    def blockIds: Seq[Tuple] = 0 until blockIdSpace
    def localThreadIds: Seq[Tuple] = 0 until localThreadIdSpace
  }
  class Config2(val blockIdSpace: (Int,Int), val localThreadIdSpace: (Int,Int)) extends IndexSpace {
    type Tuple = (Int,Int)
    def threadIdSpace = (blockIdSpace._1 * localThreadIdSpace._1, blockIdSpace._2 * localThreadIdSpace._2)

    def threadIds = for (i1 <- 0 until threadIdSpace._1; i2 <- 0 until threadIdSpace._2) yield (i1,i2)
    def blockIds = for (i1 <- 0 until blockIdSpace._1; i2 <- 0 until blockIdSpace._2) yield (i1,i2)
    def localThreadIds = for (i1 <- 0 until localThreadIdSpace._1; i2 <- 0 until localThreadIdSpace._2) yield (i1,i2)
  }
  class Config3(val blockIdSpace: (Int,Int,Int), val localThreadIdSpace: (Int,Int,Int)) extends IndexSpace {
    type Tuple = (Int,Int,Int)
    def threadIdSpace = (blockIdSpace._1 * localThreadIdSpace._1, blockIdSpace._2 * localThreadIdSpace._2, blockIdSpace._3 * localThreadIdSpace._3)

    def threadIds = for (i1 <- 0 until threadIdSpace._1; i2 <- 0 until threadIdSpace._2; i3 <- 0 until threadIdSpace._3) yield (i1,i2,i3)
    def blockIds = for (i1 <- 0 until blockIdSpace._1; i2 <- 0 until blockIdSpace._2; i3 <- 0 until blockIdSpace._3) yield (i1,i2,i3)
    def localThreadIds = for (i1 <- 0 until localThreadIdSpace._1; i2 <- 0 until localThreadIdSpace._2; i3 <- 0 until localThreadIdSpace._3) yield (i1,i2,i3)
  }

  class size(n: Int) extends StaticAnnotation

  // Want a data structure indexed by thread id with which we can access local data in another instance of the kernel
  abstract class Local[IS <: IndexSpace,A](var value: A) extends Function1[IS#Tuple, A] with Barrier {
    def apply(i: IS#Tuple): A = throw new MarkerException("Local.apply")
    def update(i: IS#Tuple, x: A): Unit = throw new MarkerException("Local.update")
    // def value_=(x: A) = { value = x }
    // def :=(x: A) = { value = x }

    // def ++(i: IS#Tuple): A = apply(id.localThread+i)
    // def --(i: IS#Tuple): A = apply(id.localThread-i)
  }

  implicit def local2a[A](x: Local[_,A]): A = x.value
  implicit def a2local[A,IS <: IndexSpace](x: A)(implicit config: IS) = Local[IS,A](x)

  case class Local1[A](value: A) extends Local[Config1,A](value)
  case class Local2[A](value: A) extends Local[Config2,A](value) with Function2[Int,Int,A] {
    def apply(i: Int, j: Int): A = super.apply((i,j))
    def update(i: Int, j: Int, x: A): Unit = super.apply((i,j))
  }
  case class Local3[A](value: A) extends Local[Config3,A](value) with Function3[Int,Int,Int,A] {
    def apply(i: Int, j: Int, k: Int): A = super.apply((i,j,k))
    def update(i: Int, j: Int, k: Int, x: A): Unit = super.update((i,j,k), x)
  }

  type Array1[A] = Array[A]
  case class Array2[A](value: Array[A], val length0: Int) extends Function2[Int,Int,A] {
    def apply(i: Int, j: Int) = value(i+length0+j)
    def update(i: Int, j: Int, x: A) = { value(i+length0+j) = x }
    def length1 = value.length / length0
  }
  case class Array3[A](value: Array[A], val length0: Int, val length1: Int) extends Function3[Int,Int,Int,A] {
    def apply(i: Int, j: Int, k: Int) = value((i+length0+j)*length1+k)
    def update(i: Int, j: Int, k: Int, x: A) = { value((i+length0+j)*length1+k) = x }
    def length2 = value.length / length0 / length1
  }

  // should really just write reduce once and do:
  // a.reduceKernel(_+_) which spawns the reduce on the gpu but completes it locally
  // need to specialize compilation of the reduce kernel to do +
  // so use the OpenCL reduceSum as a template filling in + using a #define
  // OR:
  // actually compile reduceSum written in Scala, inlining the closure passed into it

  // Need basic local arrays
  // Need basic output arrays
  // Need threadId indexed arrays
  // Need localThreadId indexed arrays

  // rename local to blockLocal
  // rename private to threadLocal

  def reduceSum(id: Id1, config: Config1)(sdata: Local1[Float], output: Local1[Float])(input: BBArray[Float]) = {
    val n = input.length
    val i = id.block * (config.numThreadsPerBlock * 2) + id.localThread

    sdata = if (i < n) input(i) else 0
    if (i + config.numThreadsPerBlock < n)
        sdata += input(i+config.numThreadsPerBlock)

    sdata.barrier

    s = config.numThreadPerBlock / 2
    while (s > 32) {
        sdata += sdata(id.localThread+s)
        sdata.barrier
        s /= 2
    }

    sdata.barrier

    if (id.localThread < 32) {
      config.numThreadsPerBlock match {
        case x if x >= 64 => sdata += sdata(id.localThread+32)   // sdata ++ 32
        case x if x >= 32 => sdata += sdata(id.localThread+16)   // sdata ++ 16
        case x if x >= 16 => sdata += sdata(id.localThread+8)    // sdata ++ 8
        case x if x >=  8 => sdata += sdata(id.localThread+4)    // sdata ++ 4
        case x if x >=  4 => sdata += sdata(id.localThread+2)    // sdata ++ 2
        case x if x >=  2 => sdata += sdata(id.localThread+1)    // sdata ++ 1
      }
    }

    if (id.localThread == 0)
      output = sdata
  }
  */

  /*
  val sum = (id: Id1, size: Id1)(a:BBArray[Float]) => {
    val localResult = new BBArray.ofDim[Float](size.group)
    var sum = 0f
    for (ai <- id.group) {
      sum += ai
    }
    sum
  }
  */
  val sum = (a:BBArray[Float]) => {
    val localResult = BBArray.ofDim[Float](1)
    var sum = 0f
    for (ai <- a) {
      sum += ai
    }
    sum
    localResult(0) = sum
    localResult
  }

  def compileMapKernel1(src: Function1[_,_], name: String): String = src match {
    case f if f == floatX2 => ("\n" +
              "__kernel void " + name + "(            \n" +
              "   __global const float* input,        \n" +
              "   __global float* output)             \n" +
              "{                                      \n" +
              "   int i = get_global_id(0);           \n" +
              "   output[i] = input[i] * 2.f;         \n" +
              "}                                      \n")
  }

  def compileMapKernel2(src: Function2[_,_,_], name: String): String = src match {
    case f if f == aSinB => ("\n" +
              "__kernel void " + name + "(            \n" +
              "   __global const float* a,            \n" +
              "   __global const float* b,            \n" +
              "   __global float* output)             \n" +
              "{                                      \n" +
              "   int i = get_global_id(0);           \n" +
              "   output[i] = a[i] * sin(b[i]) + 1.f; \n" +
              "}                                      \n")
  }

  def compileReduceKernel1(src: Function1[_,_], name: String): String = src match {
    case f if f == sum => ("\n" +
"#define T float                                                                    \n" +
"#define blockSize 128                                                              \n" +
"#define nIsPow2 1                                                                  \n" +
"__kernel void " + name + "(                                                        \n" +
"  __global const T *g_idata,                                                       \n" +
"  __global T *g_odata,                                                             \n" +
"  __local T* sdata) {                                                              \n" +
"   // perform first level of reduction,                                            \n" +
"   // reading from global memory, writing to shared memory                         \n" +
"   unsigned int n = get_global_size(0);                                            \n" +
"   unsigned int tid = get_local_id(0);                                             \n" +
"   unsigned int i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);       \n" +
"                                                                                   \n" +
"   sdata[tid] = (i < n) ? g_idata[i] : 0;                                          \n" +
"   if (i + get_local_size(0) < n)                                                  \n" +
"       sdata[tid] += g_idata[i+get_local_size(0)];                                 \n" +
"                                                                                   \n" +
"   barrier(CLK_LOCAL_MEM_FENCE);                                                   \n" +
"                                                                                   \n" +
"   // do reduction in shared mem                                                   \n" +
"   #pragma unroll 1                                                                \n" +
"   for(unsigned int s=get_local_size(0)/2; s>32; s>>=1)                            \n" +
"   {                                                                               \n" +
"       if (tid < s)                                                                \n" +
"       {                                                                           \n" +
"           sdata[tid] += sdata[tid + s];                                           \n" +
"       }                                                                           \n" +
"       barrier(CLK_LOCAL_MEM_FENCE);                                               \n" +
"   }                                                                               \n" +
"                                                                                   \n" +
"   // Question: why isn't a barrier needed here?                                   \n" +
"   if (tid < 32)                                                                   \n" +
"   {                                                                               \n" +
"       if (blockSize >=  64) { sdata[tid] += sdata[tid + 32]; }                    \n" +
"       if (blockSize >=  32) { sdata[tid] += sdata[tid + 16]; }                    \n" +
"       if (blockSize >=  16) { sdata[tid] += sdata[tid +  8]; }                    \n" +
"       if (blockSize >=   8) { sdata[tid] += sdata[tid +  4]; }                    \n" +
"       if (blockSize >=   4) { sdata[tid] += sdata[tid +  2]; }                    \n" +
"       if (blockSize >=   2) { sdata[tid] += sdata[tid +  1]; }                    \n" +
"   }                                                                               \n" +
"                                                                                   \n" +
"   // write result for this block to global mem                                    \n" +
"   if (tid == 0) g_odata[get_group_id(0)] = sdata[0];                              \n" +
" }                                                                                 \n")
  }

  var next = 0
  def freshName(base: String = "tmp") = { 
    next += 1
    base + next
  }

  implicit def f2arrayMapk1[A:FixedSizeMarshal,B:FixedSizeMarshal](f: A => B): ArrayMapKernel1[A,B] = {
    val kernelName = freshName("theKernel")
    val src = compileMapKernel1(f, kernelName)
    implicit val Ma = fixedSizeMarshal[A].manifest
    implicit val Mb = fixedSizeMarshal[B].manifest
    implicit val ma = AT[A]
    implicit val mb = AT[B]
    val kernel = CL.gpu.compile1[Array[A], Array[B]](kernelName, src,
                                                     new SimpleArrayDist1[A,Array[A]],
                                                     new SimpleGlobalArrayEffect1[A,Array[A]])
    new ArrayMapKernel1[A,B] {
      def apply(a: Array[A]) = kernel(a)
    }
  }

  implicit def f2bbarrayMapk1[A:FixedSizeMarshal,B:FixedSizeMarshal](f: A => B): BBArrayMapKernel1[A,B] = {
    val kernelName = freshName("theKernel")
    val src = compileMapKernel1(f, kernelName)
    implicit val ma = BBAT[A]
    implicit val mb = BBAT[B]
    val kernel = CL.gpu.compile1[BBArray[A], BBArray[B]](kernelName, src,
                                                         new SimpleArrayDist1[A,BBArray[A]],
                                                         new SimpleGlobalArrayEffect1[A,BBArray[A]])
    new BBArrayMapKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }

  implicit def f2bbarrayPartialReducek1[A:FixedSizeMarshal,B:FixedSizeMarshal](f: BBArray[A] => BBArray[B]): BBArrayReduceKernel1[A,BBArray[B]] = {
    val kernelName = freshName("theKernel")
    val src = compileReduceKernel1(f, kernelName)
    implicit val ma = BBAT[A]
    val numThreads = 128 // CL.gpu.device.localMemSize.toInt / 4
    println("numThreads = " + numThreads)
    val kernel = CL.gpu.compile1[BBArray[A], BBArray[B]](kernelName, src,
                                                new BlockArrayDist1[A,BBArray[A]](numThreads),
                                                new SimpleLocalArrayWithOutputEffect1[A,BBArray[A]](numThreads, numThreads * fixedSizeMarshal[A].size))
    new BBArrayReduceKernel1[A,BBArray[B]] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }

  implicit def f2bbarrayReducek1[A:FixedSizeMarshal,B:FixedSizeMarshal](f: BBArray[A] => B): BBArrayReduceKernel1[A,B] = {
    val kernelName = freshName("theKernel")
    val src = compileReduceKernel1(f, kernelName)
    implicit val ma = BBAT[A]
    val numThreads = 128 // CL.gpu.device.localMemSize.toInt / 4
    println("numThreads = " + numThreads)
    val kernel = CL.gpu.compile1[BBArray[A], B](kernelName, src,
                                                new BlockArrayDist1[A,BBArray[A]](numThreads),
                                                new SimpleLocalArrayEffect1[A,BBArray[A]](numThreads * fixedSizeMarshal[A].size))
    new BBArrayReduceKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }

/*
  implicit def f2bbarrayMapk2[A1:FixedSizeMarshal,A2:FixedSizeMarshal,B:FixedSizeMarshal](f: (A1,A2) => B): BBArrayMapKernel2[A1,A2,B] = {
    val kernelName = freshName("theKernel")
    val src = compileMapKernel1(f, kernelName)
    implicit val ma1 = BBAT[A1]
    implicit val ma2 = BBAT[A2]
    implicit val mb = BBAT[B]
    val kernel = CL.gpu.compile1[BBArray[A1], BBArray[A2], BBArray[B1]](kernelName, src,
                                                         new SimpleArrayDist1[A1,BBArray[A1]],
                                                         new SimpleGlobalArrayEffect1[A,BBArray[A]])
    new BBArrayMapKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }
*/

  trait Mem[Buf] {
    protected def alloc[A: ClassManifest](usage: CLMem.Usage, n: Int): Buf

    def allocForReadWrite[A: ClassManifest](n: Int) = alloc[A](SCLMemUsage.InputOutput, n)
    def allocForRead[A: ClassManifest](n: Int) = alloc[A](SCLMemUsage.Input, n)
    def allocForWrite[A: ClassManifest](n: Int) = alloc[A](SCLMemUsage.Output, n)
  }

  class GlobalMem(dev: Device) extends Mem[SCLBuffer[_<:NIOBuffer]] {
    protected def alloc[A: ClassManifest](usage: CLMem.Usage, n: Int): SCLBuffer[_<:NIOBuffer] = dev.context.createBuffer[A](usage, n, true)
  }

  class MarkerException(msg: String) extends RuntimeException(msg)

  trait Barrier {
    def barrier: Unit = throw new MarkerException("barrier")
  }

  class LocalMem(dev: Device) extends Mem[LocalSize] with Barrier {
    protected def alloc[A: ClassManifest](usage: CLMem.Usage, n: Int): LocalSize = new LocalSize(n)
  }

  def time[A](body: => A): A = {
    val t0 = System.currentTimeMillis
    try {
      body
    }
    finally {
      val t1 = System.currentTimeMillis
      println("time " + (t1 - t0) / 1000.)
    }
  }

  def main(args: Array[String]) = {
    System.runFinalizersOnExit(true)

    val dataSize = if (args.length > 0) args(0).toInt else 1000

    val a = Array.tabulate(dataSize)(_.toFloat)
    
    println("sequential");
    {
      val c = time {
        a.map(_ * 2.0f)
      }
    }

    println("cl array");
    {
      val c = time {
        val result = CL.gpu.spawn { a.mapKernel(floatX2) }
        result.force
      }
      assert(a.length == c.length)
      for (i <- 0 until a.length) {
        println(a(i) + " " + c(i))
        assert((a(i)*2.f - c(i)).abs < 1e-6)
      }
    }

    val b = BBArray.fromArray(a)

    println("cl bbarray");
    {
      val d = time {
        val result = CL.gpu.spawn { b.mapKernel(floatX2) }
        result.force
      }
      assert(b.length == d.length)
      for (i <- 0 until b.length) {
        println(b(i) + " " + d(i))
        assert((b(i)*2.f - d(i)).abs < 1e-6)
      }
    }

    val b2 = BBArray.tabulate(512)(_.toFloat)

    println("cl bbarray sum");
    {
      val c: BBArray[Float] = time {
        val result = CL.gpu.spawn { b2.reduceKernel(sum) }
        result.force
      }
      println("c = " + c)
      val result = c.sum
      val correct = (1 until b2.length).sum
      println("sum = " + result)
      println("correct sum = " + correct)
    }

    // New usage:
  /*
    b = gpu.spawn k(a)
    b = gpu.spawn a.map(k)
    b = gpu.spawn (a1, a2).zipMap(k)

    spawn: Kernel1[A,B] => (A => Future[B])

    (a zip b).map(gpu.spawn k).force

    (gpu.spawn k).zipMap(a, b)

    (a zip b).map(f.asKernel(gpu))

    kernel combinators!
        K * K => K
  */
  }
}
