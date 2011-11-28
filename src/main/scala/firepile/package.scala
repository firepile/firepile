import firepile.util.BufferBackedArray._

import java.nio.ByteBuffer
import java.nio.ByteOrder

import scala.reflect.Manifest

import com.nativelibs4java.opencl.CLByteBuffer
import com.nativelibs4java.opencl.CLEvent
import com.nativelibs4java.opencl.CLKernel
import com.nativelibs4java.opencl.CLKernel.LocalSize
import com.nativelibs4java.opencl.JavaCL

package object firepile {
  import Spaces._
  import Marshaling._

  val MEASURE_TIME = true

  private lazy val defaultGPUPlatform: Option[Platform] = {
    platforms.flatMap {
      (p:Platform) => p.listGPUs(true).map { (d:Device) => p }
    }.headOption
  }

  private lazy val defaultCPUPlatform: Option[Platform] = {
    platforms.flatMap {
      (p:Platform) => p.listCPUs(true).map { (d:Device) => p }
    }.headOption
  }

  private lazy val defaultGPU: Option[Device] = {
    defaultGPUPlatform match {
      case Some(p) => p.listGPUs(true).headOption
      case None => None
    }
  }

  private lazy val defaultCPU: Option[Device] = {
    defaultCPUPlatform match {
      case Some(p) => p.listCPUs(true).headOption
      case None => None
    }
  }

  private lazy val bestDevice: Device = platforms(0).bestDevice

  lazy val gpu = defaultGPU match {
    case Some(d) => d
    case None => null
  }

  implicit def BBArrayMarshal[A: FixedSizeMarshal]: BBArrayMarshal[A] = new BBArrayMarshal[A]

  def finish[A](dev: Device)(body: => A): A = {
    try {
      body
    }
    finally {
      dev.queue.finish
    }
  }

  implicit def bi12bb[A: FixedSizeMarshal](a: GroupIndexed1[A]): BBArray[A] = a.backing
  implicit def bi22bb[A: FixedSizeMarshal](a: GroupIndexed2[A]): BBArray[A] = a.backing
  implicit def bi32bb[A: FixedSizeMarshal](a: GroupIndexed3[A]): BBArray[A] = a.backing

  implicit def ti12bb[A: FixedSizeMarshal](a: ThreadIndexed1[A]): BBArray[A] = a.backing
  implicit def ti22bb[A: FixedSizeMarshal](a: ThreadIndexed2[A]): BBArray[A] = a.backing
  implicit def ti32bb[A: FixedSizeMarshal](a: ThreadIndexed3[A]): BBArray[A] = a.backing

  implicit def a2bb[A: FixedSizeMarshal](a: Array[A]): BBArray[A] = BBArray.fromArray[A](a).directCopy
  implicit def bb2a[A](b: BBArray[A])(implicit aMarshal: FixedSizeMarshal[A], Ma: ClassManifest[A]): Array[A] = Array.tabulate[A](b.length)(i => b(i))

  /*
  def spawn[B](k: Future[B])(implicit dev: Device) = k.start(dev)
  def spawn[B](dev: Device)(k: Future[B]) = k.start(dev)
  */

  // How to make kernels composable
  // Basic kernels:
  // foreach : (A => Unit) => List[A] => Unit
  // map     : (A => B) => List[A] => List[B]
  // flatMap : (A => Iterable[B]) => List[A] => List[B]
  // filter  : (A => Boolean) => List[A] => List[A]

  // zip     : (List[A], List[B]) => List[(A,B)]
  // unzip   : List[(A,B)] => (List[A], List[B])

  // zipWith : ((A,B) => C) => (List[A], List[B]) => List[C]

  // *K -- kernels, run on GPU
  // *F -- functions, run on host

  // MapK    : (A => B) => Global[A] => Global[B]
  // BlockReduceK : ((A,A) => A) => Global[A] => Block[A]
  // ReduceF : ((A,A) => A) => Block[A] => A
  // ReduceK : ((A,A) => A) => Global[A] => A

  // BlockZipReduceK : ((A,A,A) => A) => Global[A] => Block[A]

  // mkDotProduct = BlockZipReduceK o ReduceF


  // mkReduceK : ((A,A) => A) => ReduceK = (mkBlockReduceK(f) o mkReduceF(f))

  // Should copies to/from device be explicit?

  def time[A](body: => A, label: String, iterations: Int = 1): A = {
    if (MEASURE_TIME) {
      val t0 = System.nanoTime
      try {
        for (i<- 0 until iterations -1 ) body
        body
      }
      finally {
        val t1 = System.nanoTime
        println(label + " time = " + ((t1 - t0) / 1000000.) / iterations)
        Kernel.setTime(label,(t1-t0))
      }
    }
    else
      body
  }

  def platforms: List[Platform] = {
    JavaCL.listPlatforms.map(p => new Platform(p)).toList
  }

  def listGPUPoweredPlatforms: List[Platform] = {
    JavaCL.listGPUPoweredPlatforms.map(p => new Platform(p)).toList
  }

  // ------------------------------------------------------------------------
  def unloadCompiler = JavaCL.unloadCompiler

  // ------------------------------------------------------------------------

  trait HasLength[A] {
    def length(a: A): Int
  }


  def whatIsTypeName[T <: AnyRef](t: T)(implicit m: scala.reflect.Manifest[T]) = t.getClass.getName


  implicit val id1: Id1 = null
  implicit val id2: Id2 = null
  implicit val id3: Id3 = null

}
