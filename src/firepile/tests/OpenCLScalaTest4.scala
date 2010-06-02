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

  class ArrayMarshal[A](marshal: FixedSizeMarshal[A], manifest: ClassManifest[A]) extends Marshal[Array[A]] {
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

  implicit def ArrayMarshal[A](implicit marshal: FixedSizeMarshal[A], manifest: ClassManifest[A]): ArrayMarshal[A] = new ArrayMarshal[A](marshal, manifest)

  class BBArrayMarshal[A: FixedSizeMarshal] extends Marshal[BBArray[A]] {
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

  implicit def BBArrayMarshal[A: FixedSizeMarshal]: BBArrayMarshal[A] = new BBArrayMarshal[A]

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
      val lenIn = buffers.map(a => dev.global.allocForRead[Byte](fixedSizeMarshal[Int].size))
      val memOut = dev.global.allocForWrite[Byte](e.outputSize)
      val lenOut = dev.global.allocForRead[Byte](fixedSizeMarshal[Int].size)

      val writeEvents = (buffers zip memIn).map{ case (a,mem) => mem.write(dev.queue, Buffer.fromNIOBuffer[Byte](a), false) }
      val writeLenEvents = (buffers zip lenIn).map{ case (a,mem) => mem.write(dev.queue, {
          val b = ByteBuffer.allocate(fixedSizeMarshal[Int].size).order(ByteOrder.nativeOrder)
          b.putInt(a.limit)
          Buffer.fromNIOBuffer[Byte](b)
        }, false) }

      val writeOutLenEvent = lenOut.write(dev.queue, {
          val b = ByteBuffer.allocate(fixedSizeMarshal[Int].size).order(ByteOrder.nativeOrder)
          b.putInt(e.outputSize)
          Buffer.fromNIOBuffer[Byte](b)
        }, false)

      val localMem: List[LocalSize] = e.localBufferSizes.map(size => dev.local.allocForReadWrite[Byte](size))
      val args0: List[SCLBuffer[_]] = (memIn zip lenIn).flatMap[SCLBuffer[_], Seq[SCLBuffer[_]]]{ case (mem,n) => mem::n::Nil }.toList
      val args1: List[SCLBuffer[_]] = memOut :: lenOut :: Nil
      val args = args0 ::: args1 ::: localMem
      println(args)
      code.setArgs(args:_*)

      // println(args)

      val runEvent = code.enqueueNDRange(dev.queue, Array(d.totalNumberOfItems), Array(d.numberOfItemsPerGroup),
                (writeEvents.toList ::: writeLenEvents.toList ::: writeOutLenEvent :: Nil):_*)

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
      // Round up to next block size
      val totalNumberOfItems = (a.length + n - 1) / n * n
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
      def reduceKernel(k: ArrayReduceKernel1[A]) = k(a)
    }
    class BBArrayKernelWrapper[A](a: BBArray[A]) {
      def mapKernel[B](k: BBArrayMapKernel1[A,B]) = k(a)
      def reduceKernel(k: BBArrayReduceKernel1[A]) = k(a)
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

    trait ArrayReduceKernel1[A] extends Kernel1[Array[A],A]
    trait BBArrayReduceKernel1[A] extends Kernel1[BBArray[A],A]

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

  trait IdSpace[Pt <: Point[Pt]] extends Iterable[Pt] {
    def extent: Pt
    def length: Int
    def index(p: Pt): Int
  }
  class IdSpace1(val extent: Point1) extends IdSpace[Point1] {
    def iterator = (for (i <- 0 until extent.x) yield Point1(i)).toIterator
    def length = extent.x
    def index(p: Point1) = {
      val q = p | this
      q.x
    }
  }
  class IdSpace2(val extent: Point2) extends IdSpace[Point2] {
    def iterator = (for (i1 <- 0 until extent.x; i2 <- 0 until extent.y) yield Point2(i1, i2)).toIterator
    def length = extent.x * extent.y
    def index(p: Point2) = {
      val q = p | this
      q.x * extent.x + q.y
    }
  }
  class IdSpace3(val extent: Point3) extends IdSpace[Point3] {
    def iterator = (for (i1 <- 0 until extent.x; i2 <- 0 until extent.y; i3 <- 0 until extent.z) yield Point3(i1, i2, i3)).toIterator
    def length = extent.x * extent.y * extent.z
    def index(p: Point3) = {
      val q = p | this
      (q.x * extent.x + q.y) * extent.y + q.z
    }
  }

  trait Point[Pt <: Point[Pt]] {
    this: Pt =>

    def rank: Int

    def +(i: Int): Pt
    def -(i: Int): Pt
    def /(i: Int): Pt
    def %(i: Int): Pt
    def *(i: Int): Pt

    def +(p: Pt): Pt
    def -(p: Pt): Pt
    def *(p: Pt): Pt
    def /(p: Pt): Pt
    def %(p: Pt): Pt

    def scale(b: Pt, n: Pt, t: Pt): Pt = b * n + t

    def |(s: IdSpace[Pt]): Pt
  }
  case class Point1(x: Int) extends Point[Point1] {
    def rank = 1

    def toInt = x

    def +(i: Int) = Point1(x+i)
    def -(i: Int) = Point1(x-i)
    def /(i: Int) = Point1(x/i)
    def %(i: Int) = Point1(x%i)
    def *(i: Int) = Point1(x*i)

    def +(p: Point1) = Point1(x+p.x)
    def -(p: Point1) = Point1(x-p.x)
    def *(p: Point1) = Point1(x*p.x)
    def /(p: Point1) = Point1(x/p.x)
    def %(p: Point1) = Point1(x%p.x)

    def |(s: IdSpace[Point1]) = Point1(x % s.extent.x)
  }
  case class Point2(x: Int, y: Int) extends Point[Point2] {
    def rank = 2

    def toTuple2 = (x,y)

    def +(i: Int) = Point2(x+i,y+i)
    def -(i: Int) = Point2(x-i,y-i)
    def /(i: Int) = Point2(x/i, y/i)
    def %(i: Int) = Point2(x%i, y%i)
    def *(i: Int) = Point2(x*i, y*i)

    def +(i: Int, j: Int) = Point2(x+i,y+j)
    def -(i: Int, j: Int) = Point2(x-i,y-j)
    def *(i: Int, j: Int) = Point2(x*i,y*j)
    def /(i: Int, j: Int) = Point2(x/i,y/j)
    def %(i: Int, j: Int) = Point2(x%i,y%j)

    def +(p: Point2) = Point2(x+p.x,y+p.y)
    def -(p: Point2) = Point2(x-p.x,y-p.y)
    def *(p: Point2) = Point2(x*p.x, y*p.y)
    def /(p: Point2) = Point2(x/p.x, y/p.y)
    def %(p: Point2) = Point2(x%p.x, y%p.y)

    def |(s: IdSpace[Point2]) = Point2(x % s.extent.x, y % s.extent.y)
  }
  case class Point3(x: Int, y: Int, z: Int) extends Point[Point3] {
    def rank = 3

    def toTuple3 = (x,y,z)

    def +(i: Int) = Point3(x+i,y+i,z+i)
    def -(i: Int) = Point3(x-i,y-i,z-i)
    def /(i: Int) = Point3(x/i, y/i, z/i)
    def %(i: Int) = Point3(x%i, y%i, z%i)
    def *(i: Int) = Point3(x*i, y*i, z*i)

    def +(i: Int, j: Int, k: Int) = Point3(x+i,y+j,z+k)
    def -(i: Int, j: Int, k: Int) = Point3(x-i,y-j,z-k)
    def *(i: Int, j: Int, k: Int) = Point3(x*i,y*j,z*k)
    def /(i: Int, j: Int, k: Int) = Point3(x/i,y/j,z/k)
    def %(i: Int, j: Int, k: Int) = Point3(x%i,y%j,z%k)

    def +(p: Point3) = Point3(x+p.x,y+p.y,z+p.z)
    def -(p: Point3) = Point3(x-p.x,y-p.y,z-p.z)
    def *(p: Point3) = Point3(x*p.x,y*p.y,z*p.z)
    def /(p: Point3) = Point3(x/p.x,y/p.y,z/p.z)
    def %(p: Point3) = Point3(x%p.x,y%p.y,z%p.z)

    def |(s: IdSpace[Point3]) = Point3(x % s.extent.x, y % s.extent.y, z % s.extent.z)
  }

  implicit def int2point1(p: Int) = Point1(p)
  implicit def int2point2(p: (Int,Int)) = Point2(p._1,p._2)
  implicit def int2point3(p: (Int,Int,Int)) = Point3(p._1,p._2,p._3)
  implicit def point12int(p: Point1) = p.x
  implicit def point22int(p: Point2) = (p.x,p.y)
  implicit def point32int(p: Point3) = (p.x,p.y,p.z)

  class Ident[Pt <: Point[Pt]](val config: Config[Pt], val block: Pt, val localThread: Pt) {
    def thread: Pt = block * config.localThreadIdSpace.extent + localThread
    def global = thread
  }

  class Id1(config: Config1, block: Point1, localThread: Point1) extends Ident[Point1](config, block, localThread)
  class Id2(config: Config2, block: Point2, localThread: Point2) extends Ident[Point2](config, block, localThread)
  class Id3(config: Config3, block: Point3, localThread: Point3) extends Ident[Point3](config, block, localThread)

  /** Index space */
  trait Config[Pt <: Point[Pt]] {
    /** Space of global thread ids; must be blockIdSpace * localThreadIdSpace */
    def threadIdSpace: IdSpace[Pt]
    /** Space of block (work group) ids */
    def blockIdSpace: IdSpace[Pt]
    /** Space of local thread (work item) ids witin a block.  All blocks have the same local space. */
    def localThreadIdSpace: IdSpace[Pt]

    def threadIds = threadIdSpace.iterator
    def blockIds = blockIdSpace.iterator
    def localThreadIds = localThreadIdSpace.iterator

    /** Return the block ID of a given global thread ID */
    def blockIdOfThread(p: Pt) = p / blockIdSpace.extent
    def localThreadIdOfThread(p: Pt) = p % blockIdSpace.extent

    def numThreads = threadIdSpace.length
    def numBlocks = blockIdSpace.length
    def numThreadsPerBlock = localThreadIdSpace.length
  }

  class Config1(val maxBlock: Point1, val maxLocalThread: Point1) extends Config[Point1] {
    type Pt = Point1
    def threadIdSpace = new IdSpace1(maxBlock * maxLocalThread)
    def blockIdSpace = new IdSpace1(maxBlock)
    def localThreadIdSpace = new IdSpace1(maxLocalThread)
  }
  class Config2(val maxBlock: Point2, val maxLocalThread: Point2) extends Config[Point2] {
    type Pt = Point2
    def threadIdSpace = new IdSpace2(maxBlock * maxLocalThread)
    def blockIdSpace = new IdSpace2(maxBlock)
    def localThreadIdSpace = new IdSpace2(maxLocalThread)
  }
  class Config3(val maxBlock: Point3, val maxLocalThread: Point3) extends Config[Point3] {
    type Pt = Point3
    def threadIdSpace = new IdSpace3(maxBlock * maxLocalThread)
    def blockIdSpace = new IdSpace3(maxBlock)
    def localThreadIdSpace = new IdSpace3(maxLocalThread)
  }

  class Indexed[Pt <: Point[Pt], A: ClassManifest](space: IdSpace[Pt]) {
    private val backing: Array[A] = Array.ofDim[A](space.length)
    def apply(p: Pt): A = backing(space.index(p))
    def update(p: Pt, x: A): Unit = { backing(space.index(p)) = x }
  }
  class LocalThreadIndexed[Pt <: Point[Pt], A: ClassManifest](config: Config[Pt]) extends Indexed[Pt,A](config.localThreadIdSpace) with Barrier
  class LocalThreadIndexed1[A: ClassManifest](config: Config1) extends LocalThreadIndexed[Point1, A](config)
  class LocalThreadIndexed2[A: ClassManifest](config: Config2) extends LocalThreadIndexed[Point2, A](config)
  class LocalThreadIndexed3[A: ClassManifest](config: Config3) extends LocalThreadIndexed[Point3, A](config)

  class BlockIndexed[Pt <: Point[Pt], A: ClassManifest](config: Config[Pt]) extends Indexed[Pt,A](config.blockIdSpace)
  class BlockIndexed1[A: ClassManifest](config: Config1) extends BlockIndexed[Point1, A](config)
  class BlockIndexed2[A: ClassManifest](config: Config2) extends BlockIndexed[Point2, A](config)
  class BlockIndexed3[A: ClassManifest](config: Config3) extends BlockIndexed[Point3, A](config)

  def reduceSum(id: Id1, config: Config1)(sdata: LocalThreadIndexed1[Float], output: BlockIndexed1[Float])(input: BBArray[Float]) = {
    val n = input.length
    val i = id.block * (config.numThreadsPerBlock * 2) + id.localThread
    val tid = id.localThread

    sdata(tid) = if (i < n) input(i) else 0
    if (i + config.numThreadsPerBlock < n)
        sdata(tid) += input(i+config.numThreadsPerBlock)

    sdata.barrier

    var s = config.numThreadsPerBlock / 2
    while (s > 32) {
        sdata(tid) += sdata(tid+s)
        sdata.barrier
        s /= 2
    }

    sdata.barrier

    if (tid < 32) {
      config.numThreadsPerBlock match {
        case x if x >= 64 => sdata(tid) += sdata(tid+32)
        case x if x >= 32 => sdata(tid) += sdata(tid+16)
        case x if x >= 16 => sdata(tid) += sdata(tid+8)
        case x if x >=  8 => sdata(tid) += sdata(tid+4)
        case x if x >=  4 => sdata(tid) += sdata(tid+2)
        case x if x >=  2 => sdata(tid) += sdata(tid+1)
      }
    }

    if (tid == 0)
      output(id.block) = sdata(tid)
  }

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
              "   int input_len,                      \n" +
              "   __global float* output,             \n" +
              "   int output_len)                     \n" +
              "{                                      \n" +
              "   int i = get_global_id(0);           \n" +
              "   output[i] = input[i] * 2.f;         \n" +
              "}                                      \n")
  }

  def compileMapKernel2(src: Function2[_,_,_], name: String): String = src match {
    case f if f == aSinB => ("\n" +
              "__kernel void " + name + "(            \n" +
              "   __global const float* a,            \n" +
              "   int a_len,                          \n" +
              "   __global const float* b,            \n" +
              "   int b_len,                          \n" +
              "   __global float* output,             \n" +
              "   int output_len)                     \n" +
              "{                                      \n" +
              "   int i = get_global_id(0);           \n" +
              "   output[i] = a[i] * sin(b[i]) + 1.f; \n" +
              "}                                      \n")
  }

  val sumFloat: (Float,Float) => Float = (_+_)

  def compileReduceKernel1(src: Function2[_,_,_], name: String): String = src match {
    case f if f == sumFloat => ("\n" +
"#define T float                                                                    \n" +
"#define blockSize 128                                                              \n" +
"#define f(a,b) (a+b)                                                               \n" +
"__kernel void " + name + "(                                                        \n" +
"  __global const T *g_idata, /* thread indexed */                                  \n" +
"  const int g_idata_length,                                                       \n" +
"  __global T *g_odata,       /* block indexed */                                   \n" +
"  const int g_odata_length,                                                       \n" +
"  __local T* sdata) {        /* local thread indexed */                            \n" +
"   // perform first level of reduction,                                            \n" +
"   // reading from global memory, writing to local memory                          \n" +
"   unsigned int n = g_idata_length;                                            \n" +
"   unsigned int tid = get_local_id(0);                                             \n" +
"   unsigned int i = get_global_id(0); // get_group_id(0)*get_local_size(0) + get_local_id(0);           \n" +
"                                                                                   \n" +
"   sdata[tid] = (i < n) ? g_idata[i] : 0;                                                               \n" +
"                                                                                   \n" +
"   barrier(CLK_LOCAL_MEM_FENCE);                                                   \n" +
"                                                                                   \n" +
"   // do reduction in shared mem                                                   \n" +
"   #pragma unroll 1                                                                \n" +
"   for(unsigned int s=get_local_size(0)/2; s>0; s>>=1)                             \n" +
"   {                                                                               \n" +
"       if (tid < s)                                                                \n" +
"       {                                                                           \n" +
"           sdata[tid] = f(sdata[tid], sdata[tid+s]);                                             \n" +
"       }                                                                           \n" +
"       barrier(CLK_LOCAL_MEM_FENCE);                                               \n" +
"   }                                                                               \n" +
"                                                                                   \n" +
"   // write result for this block to global mem                                    \n" +
"   if (tid == 0)                                                                   \n" +
"       g_odata[get_group_id(0)] = sdata[0];                                        \n" +
"}                                                                                  \n")
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
    implicit val ma = ArrayMarshal[A]
    implicit val mb = ArrayMarshal[B]
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
    implicit val ma = BBArrayMarshal[A]
    implicit val mb = BBArrayMarshal[B]
    val kernel = CL.gpu.compile1[BBArray[A], BBArray[B]](kernelName, src,
                                                         new SimpleArrayDist1[A,BBArray[A]],
                                                         new SimpleGlobalArrayEffect1[A,BBArray[A]])
    new BBArrayMapKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }

  implicit def f2bbarrayPartialReducek1[A:FixedSizeMarshal](f: (A,A) => A): BBArrayReduceKernel1[A] = {
    val kernelName = freshName("theKernel")
    val src = compileReduceKernel1(f, kernelName)
    implicit val ma = BBArrayMarshal[A]
    val numThreads = 128 // CL.gpu.device.localMemSize.toInt / 4
    println("numThreads = " + numThreads)
    val d = new BlockArrayDist1[A,BBArray[A]](numThreads)
    val e = new SimpleLocalArrayWithOutputEffect1[A,BBArray[A]](numThreads, numThreads * fixedSizeMarshal[A].size)
    println(d)
    println(e)
    val kernel = CL.gpu.compile1[BBArray[A], BBArray[A]](kernelName, src, d, e)
    new BBArrayReduceKernel1[A] {
      def apply(a: BBArray[A]) = new InstantiatedKernel[A] {
        def run(dev: Device): Future[A] = {
          val future: Future[BBArray[A]] = kernel(a).run(dev)
          new Future[A] {
            def force: A = {
              val result = future.force
              println("reduce result = " + result)
              result.reduceLeft(f)
            }
          }
        }
      }
    }
  }

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
      val c: Float = time {
        val result = CL.gpu.spawn { b2.reduceKernel(sumFloat) }
        result.force
      }
      println("c = " + c)
      val correct = b2.sum
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
