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
  trait Kernel1[A,B] extends Function1[A,InstantiatedKernel1[A,B]] with Kernel {
  }
  trait Kernel2[A1,A2,B] extends Function2[A1,A2,InstantiatedKernel2[A1,A2,B]] with Kernel {
  }
  trait Kernel3[A1,A2,A3,B] extends Function3[A1,A2,A3,InstantiatedKernel3[A1,A2,A3,B]] with Kernel {
  }

  trait InstantiatedKernel[B] {
    def run(dev: Device): Future[B]
  }
  trait InstantiatedKernel1[A,B] extends InstantiatedKernel[B] { }
  trait InstantiatedKernel2[A1,A2,B] extends InstantiatedKernel[B] { }
  trait InstantiatedKernel3[A1,A2,A3,B] extends InstantiatedKernel[B] { }


  class BufKernel1(code: SCLKernel, val dist: Disted, val effect: Effect) extends Kernel1[ByteBuffer,ByteBuffer] {
    def apply(a: ByteBuffer) = new InstantiatedBufKernel1(code, a, dist, effect)
  }
  /*
  class BufKernel2(code: SCLKernel, val dist: Disted, effect: Effect) extends Kernel2[ByteBuffer,ByteBuffer,ByteBuffer] {
    def apply(a1: ByteBuffer, a2: ByteBuffer) = new InstantiatedBufKernel2(code, a1, a2, dist, effect)
  }
  class BufKernel3(code: SCLKernel, val dist: Disted, effect: Effect) extends Kernel3[ByteBuffer,ByteBuffer,ByteBuffer,ByteBuffer] {
    def apply(a1: ByteBuffer, a2: ByteBuffer, a3: ByteBuffer) = new InstantiatedBufKernel3(code, a1, a2, a3, dist, effect)
  }
  */

  abstract class InstantiatedBufKernel {
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

  val printBuffers = false

  def printBuffer(a: => ByteBuffer) = if (printBuffers) {
    var bb = a.asFloatBuffer.duplicate
    var i = 0
    while (bb.hasRemaining) {
      println(i + ": " + bb.get)
      i += 1
    }
  }

  class InstantiatedBufKernel1(code: SCLKernel, a: ByteBuffer, val disted: Disted, val effect: Effect) extends InstantiatedBufKernel with InstantiatedKernel1[ByteBuffer, ByteBuffer] {
    def run(dev: Device) = {
      val d = disted
      val e = effect
      val memIn = dev.global.allocForRead[Byte](a.limit)
      val memOut = dev.global.allocForWrite[Byte](e.outputSize)

      if (printBuffers) println(d)
      if (printBuffers) println(e)
      if (printBuffers) println("input " + a.limit)
      printBuffer(a)

      val writeEvent = memIn.write(dev.queue, Buffer.fromNIOBuffer[Byte](a), false)

      val args = memIn :: memOut :: e.localBufferSizes.map(size => dev.local.allocForReadWrite[Byte](size))
      code.setArgs(args:_*)

      // println(args)

      val runEvent = code.enqueueNDRange(dev.queue, Array(d.totalNumberOfItems), Array(d.numberOfItemsPerGroup), writeEvent)

      readBackResult(dev, e.outputSize, runEvent, memOut)
    }
  }

  class InstantiatedBufKernel2(code: SCLKernel, a1: ByteBuffer, a2: ByteBuffer, val disted: Disted, val effect: Effect) extends InstantiatedBufKernel with InstantiatedKernel2[ByteBuffer, ByteBuffer, ByteBuffer] {
    def run(dev: Device) = {
      val d = disted
      val e = effect
      val memIn1 = dev.global.allocForRead[Byte](a1.limit)
      val memIn2 = dev.global.allocForRead[Byte](a2.limit)
      val memOut = dev.global.allocForWrite[Byte](e.outputSize)

      val writeEvent1 = memIn1.write(dev.queue, Buffer.fromNIOBuffer[Byte](a1), false)
      val writeEvent2 = memIn2.write(dev.queue, Buffer.fromNIOBuffer[Byte](a2), false)

      val args = memIn1 :: memIn2 :: memOut :: e.localBufferSizes.map(size => dev.local.allocForReadWrite[Byte](size))
      code.setArgs(args:_*)

      val runEvent = code.enqueueNDRange(dev.queue, Array(d.totalNumberOfItems), Array(d.numberOfItemsPerGroup), writeEvent1, writeEvent2)

      readBackResult(dev, e.outputSize, runEvent, memOut)
    }
  }

  class InstantiatedBufKernel3(code: SCLKernel, a1: ByteBuffer, a2: ByteBuffer, a3: ByteBuffer, val disted: Disted, val effect: Effect) extends InstantiatedBufKernel with InstantiatedKernel3[ByteBuffer, ByteBuffer, ByteBuffer, ByteBuffer] {
    def run(dev: Device) = {
      val d = disted
      val e = effect
      val memIn1 = dev.global.allocForRead[Byte](a1.limit)
      val memIn2 = dev.global.allocForRead[Byte](a2.limit)
      val memIn3 = dev.global.allocForRead[Byte](a3.limit)
      val memOut = dev.global.allocForWrite[Byte](e.outputSize)

      val writeEvent1 = memIn1.write(dev.queue, Buffer.fromNIOBuffer[Byte](a1), false)
      val writeEvent2 = memIn2.write(dev.queue, Buffer.fromNIOBuffer[Byte](a2), false)
      val writeEvent3 = memIn2.write(dev.queue, Buffer.fromNIOBuffer[Byte](a3), false)

      val args = memIn1 :: memIn2 :: memIn3 :: memOut :: e.localBufferSizes.map(size => dev.local.allocForReadWrite[Byte](size))
      code.setArgs(args:_*)

      val runEvent = code.enqueueNDRange(dev.queue, Array(d.totalNumberOfItems), Array(d.numberOfItemsPerGroup), writeEvent1, writeEvent2, writeEvent3)

      readBackResult(dev, e.outputSize, runEvent, memOut)
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

  trait Disted {
    def totalNumberOfItems: Int
    def numberOfItemsPerGroup: Int = 1
    override def toString = "Disted {n=" + totalNumberOfItems + "/" + numberOfItemsPerGroup + "}"
  }

  type Dist1[A] = Function1[A,Disted]
  type Dist2[A1,A2] = Function2[A1,A2,Disted]
  type Dist3[A1,A2,A3] = Function3[A1,A2,A3,Disted]

  type Effect1[A] = Function1[A,Effect]
  type Effect2[A1,A2] = Function2[A1,A2,Effect]
  type Effect3[A1,A2,A3] = Function3[A1,A2,A3,Effect]

  class SimpleArrayDist1[A:FixedSizeMarshal,T <: { def length: Int }] extends Dist1[T] {
    def apply(a: T) = new Disted {
      val totalNumberOfItems = a.length
    }
  }

  class SimpleGlobalArrayEffect1[A:FixedSizeMarshal, T <: { def length: Int }] extends Effect1[T] {
    def apply(a: T) = new Effect {
      val outputSize = a.length * fixedSizeMarshal[A].size
    }
  }

  class SimpleLocalArrayEffect1[A:FixedSizeMarshal](n:Int) extends Effect1[Array[A]] {
    def apply(a: Array[A]) = new Effect {
      val outputSize = a.length * fixedSizeMarshal[A].size
      override val localBufferSizes = List[Int](n)
    }
  }

  class Device(val platform: SCLPlatform, val device: SCLDevice, val context: SCLContext) {
    private def compileBuffer1(name: String, src: String): (Disted,Effect) => BufKernel1 = {
      val program = context.createProgram(src).build
      val code = program.createKernel(name)
      (d: Disted, e: Effect) => new BufKernel1(code, d, e)
    }

    def compile1[A: Marshal, B: Marshal](name: String, src: String, dist: Dist1[A], effect: Effect1[A]) = {
      val transA = implicitly[Marshal[A]];
      val transB = implicitly[Marshal[B]];

      val kernel: (Disted,Effect) => BufKernel1 = compileBuffer1(name, src)

      new Kernel1[A,B] {
        def apply(input: A) = new InstantiatedKernel1[A,B] {
          def run(dev: Device) = {
            val bufIn: ByteBuffer = transA.put(input)
            val d: Disted = dist(input)
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

    lazy val global: Mem = new Mem(this)
    lazy val local: Mem = new Mem(this)

    def spawn[A,B](k: InstantiatedKernel1[A,B]) = k.run(this)
    def spawn[A1,A2,B](k: InstantiatedKernel2[A1,A2,B]) = k.run(this)
    def spawn[A1,A2,A3,B](k: InstantiatedKernel3[A1,A2,A3,B]) = k.run(this)
  }

    // Kernels that were compiled from functions on single elements
    trait ArrayMapKernel1[A,B] extends Kernel1[Array[A],Array[B]]
    trait ArrayMapKernel2[A1,A2,B] extends Kernel2[Array[A1],Array[A2],Array[B]]
    trait ArrayMapKernel3[A1,A2,A3,B] extends Kernel3[Array[A1],Array[A2],Array[A3],Array[B]]
    trait BBArrayMapKernel1[A,B] extends Kernel1[BBArray[A],BBArray[B]]
    trait BBArrayMapKernel2[A1,A2,B] extends Kernel2[BBArray[A1],BBArray[A2],BBArray[B]]
    trait BBArrayMapKernel3[A1,A2,A3,B] extends Kernel3[BBArray[A1],BBArray[A2],BBArray[A3],BBArray[B]]

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

  def compileMapKernel(src: Object, name: String): String = src match {
    case f if f == floatX2 => ("\n" +
              "__kernel void " + name + "(            \n" +
              "   __global const float* input,        \n" +
              "   __global float* output)             \n" +
              "{                                      \n" +
              "   int i = get_global_id(0);           \n" +
              "   output[i] = input[i] * 2.0f;        \n" +
              "}                                      \n")
    case f if f == aSinB => ("\n" +
              "__kernel void " + name + "(            \n" +
              "   __global const float* input,        \n" +
              "   __global float* output)             \n" +
              "{                                      \n" +
              "   int i = get_global_id(0);           \n" +
              "   output[i] = input[i] * 2.0f;        \n" +
              "}                                      \n")
  }

  var next = 0
  def freshName(base: String = "tmp") = { 
    next += 1
    base + next
  }

  implicit def f2arrayMapk1[A:FixedSizeMarshal,B:FixedSizeMarshal](f: A => B): ArrayMapKernel1[A,B] = {
    val kernelName = freshName("theKernel")
    val src = compileMapKernel(f, kernelName)
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
    val src = compileMapKernel(f, kernelName)
    implicit val ma = BBAT[A]
    implicit val mb = BBAT[B]
    val kernel = CL.gpu.compile1[BBArray[A], BBArray[B]](kernelName, src,
                                                         new SimpleArrayDist1[A,BBArray[A]],
                                                         new SimpleGlobalArrayEffect1[A,BBArray[A]])
    new BBArrayMapKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }

  class Mem(dev: Device) {
    lazy val context = dev.context
    private def alloc[A: ClassManifest](usage: CLMem.Usage, n: Int): SCLBuffer[_<:NIOBuffer] = context.createBuffer[A](usage, n, true)
    def alloc[A: ClassManifest](n: Int): SCLBuffer[_<:NIOBuffer] = allocForReadWrite[A](n)
    def allocForReadWrite[A: ClassManifest](n: Int) = alloc[A](SCLMemUsage.InputOutput, n)
    def allocForRead[A: ClassManifest](n: Int) = alloc[A](SCLMemUsage.Input, n)
    def allocForWrite[A: ClassManifest](n: Int) = alloc[A](SCLMemUsage.Output, n)
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

    val src = "\n" +
              "__kernel void copyVec(                         \n" +
              "   __global const float* input,                \n" +
              "   __global float* output)                     \n" +
              "{                                              \n" +
              "   int i = get_global_id(0);                   \n" +
              "   output[i] = input[i] * 2.0f;                \n" +
              "}                                              \n"

    val a = Array.tabulate(dataSize)(_.toFloat)
    
    println("sequential");
    {
      val c = time {
        a.map(_ * 2.0f)
      }
    }

    val ak: Kernel1[Array[Float],Array[Float]] = floatX2

    println("cl array");
    {
      val c = time {
        val result = CL.gpu.spawn { ak(a) }
        result.force
      }
      assert(a.length == c.length)
      for (i <- 0 until a.length) {
        println(a(i) + " " + c(i))
        assert((a(i)*2.f - c(i)).abs < 1e-6)
      }
    }

    val b = BBArray.fromArray(a)
    val bk: Kernel1[BBArray[Float],BBArray[Float]] = floatX2

    println("cl bbarray");
    {
      val d = time {
        val result = CL.gpu.spawn { bk(b) }
        result.force
      }
      assert(b.length == d.length)
      for (i <- 0 until b.length) {
        println(b(i) + " " + d(i))
        assert((b(i)*2.f - d(i)).abs < 1e-6)
      }
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
