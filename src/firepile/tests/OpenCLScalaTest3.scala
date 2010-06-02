package simplecl.tests

import simplecl._

import java.nio.FloatBuffer
import simplecl.util.Buffer
import scala.reflect.Manifest
import com.nativelibs4java.opencl.CLMem

object OpenCLScalaTest3 {
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

		private lazy val gpuContext = (defaultGPUPlatform, defaultGPU) match {
			case (Some(p), Some(d)) => Some(p.createContext(null, d))
			case _ => None
		}

		lazy val gpu = gpuContext match {
			case Some(c) => new Device(c)
			case None => null
		}
	}

	/** Translate from A to Buffer[BaseType].  BaseType is hidden. */
	trait Translator[A] {
		type BaseType // <: ClassManifest[BaseType]
		def manifest: ClassManifest[BaseType]
		def size(a: A): Int
		def align: Int
		def copyIn(dev: Device, a: A): Buffer[BaseType]
		def copyOut(dev: Device, b: Buffer[BaseType]): A
	}
	
	abstract class PT[A: ClassManifest] extends Translator[A] {
		type BaseType = A
		val manifest = implicitly[ClassManifest[A]] 
		
		def copyIn(dev: Device, a: A): Buffer[A] = {
			val b = Buffer.make[A](1)
			b.put(a)
			b
		}
		
		def copyOut(dev: Device, b: Buffer[A]): A = b.get
	}
	
	implicit object BT extends PT[Byte] {
		def size(a: Byte) = 1
		def align = 1
	}
	implicit object ST extends PT[Short] {
		def size(a: Short) = 2
		def align = 2
	}
	implicit object CT extends PT[Char] {
		def size(a: Char) = 2
		def align = 2
	}
	implicit object IT extends PT[Int] {
		def size(a: Int) = 4
		def align = 4
	}
	implicit object LT extends PT[Long] {
		def size(a: Long) = 8
		def align = 8
	}
	implicit object FT extends PT[Float] { 
		def size(a: Float) = 4
		def align = 4
	}
	implicit object DT extends PT[Double] { 
		def size(a: Double) = 8
		def align = 8
	}

	abstract class AT[A: ClassManifest](t: PT[A]) extends Translator[Array[A]] { 
		type BaseType = A
		val manifest = t.manifest

		def size(a: Array[A]) = if (a.length == 0) 0 else t.size(a(0)) * a.length
		def align = t.align
		def copyIn(dev: Device, a: Array[A]) = Buffer.fromArray[A](a)
		def copyOut(dev: Device, b: Buffer[A]) = {
			if (b.hasArray) {
				b.asArray
			}
			else {
				val a = Array.ofDim[A](b.capacity)
				b.get(a)
				a
			}
		}
	}

	implicit object BAT extends AT[Byte](BT) { } 
	implicit object SAT extends AT[Short](ST) { } 
	implicit object CAT extends AT[Char](CT) { } 
	implicit object IAT extends AT[Int](IT) { } 
	implicit object LAT extends AT[Long](LT) { } 
	implicit object FAT extends AT[Float](FT) { } 
	implicit object DAT extends AT[Double](DT) { } 

	trait Future[B] {
		def force: B
	}

	trait Kernel1[A, B] extends Function1[A,RunnableKernel1[A,B]] { }

	class BufKernel1[A: ClassManifest, B: ClassManifest](code: SCLKernel) extends Kernel1[Buffer[A], Buffer[B]] {
		def apply(input: Buffer[A]) = new RunnableBufKernel1[A,B](code, input)
	}

	trait RunnableKernel1[A,B] {
		def run(dev: Device, totalNumberOfItems: Int, numberOfItemsPerGroup: Int): Future[B]
	}

	class RunnableBufKernel1[A: ClassManifest, B: ClassManifest](code: SCLKernel, input: Buffer[A]) extends RunnableKernel1[Buffer[A], Buffer[B]] {
		def run(dev: Device, totalNumberOfItems: Int, numberOfItemsPerGroup: Int) = {
			val length = totalNumberOfItems // input.capacity

			val memIn = dev.global.allocForRead[A](length)
			val memOut = dev.global.allocForWrite[B](length)

			val writeEvent = memIn.write(dev.queue, input, true)

			code.setArgs(memIn, memOut)

			val runEvent = code.enqueueNDRange(dev.queue, Array(totalNumberOfItems), Array(numberOfItemsPerGroup), writeEvent)

			val bufOut = Buffer.makeDirect[B](length)
			val readEvent = memOut.read(dev.queue, bufOut, true, runEvent)

			new Future[Buffer[B]] {
				def force = {
						dev.queue.enqueueWaitForEvents(readEvent)

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

	class Device(val context: SCLContext) {
		def compileBuffer1[A: ClassManifest,B: ClassManifest](name: String, src: String): Kernel1[Buffer[A],Buffer[B]] = {
				val program = context.createProgram(src).build
				val code = program.createKernel(name)
				new BufKernel1[A,B](code)
		}

		def compile1[A: Translator, B: Translator](name: String, src: String) = {
			val transA = implicitly[Translator[A]];
			val transB = implicitly[Translator[B]];
			val mA = transA.manifest
			val mB = transB.manifest
			val kernel = compileBuffer1[transA.BaseType,transB.BaseType](name, src)(mA, mB)

			new Kernel1[A,B] {
				def apply(input: A) = new RunnableKernel1[A,B] {
					def run(dev: Device, totalNumberOfItems: Int, numberOfItemsPerGroup: Int) = {
						val bufIn: Buffer[transA.BaseType] = transA.copyIn(dev, input)

						val f = dev.spawn(totalNumberOfItems, numberOfItemsPerGroup) { kernel(bufIn) }

					new Future[B] {
						def force = {
								val bufOut: Buffer[transB.BaseType] = f.force
								val result: B = transB.copyOut(dev, bufOut)
								result
						}
					}
					}
				}
			}
		}

		lazy val queue = context.createDefaultQueue()

		lazy val global: Mem = new Mem(this)

		def spawn[A,B](totalNumberOfItems: Int)(k: RunnableKernel1[A,B]) = k.run(this, totalNumberOfItems, 1)
		def spawn[A,B](totalNumberOfItems: Int, numberOfItemsPerGroup: Int)(k: RunnableKernel1[A,B]) =
			k.run(this, totalNumberOfItems, numberOfItemsPerGroup)
	}

	class Mem(dev: Device) {
		lazy val context = dev.context
		private def alloc[A: ClassManifest](usage: CLMem.Usage, n: Int): SCLBuffer[_<:java.nio.Buffer] = context.createBuffer[A](usage, n, true)
		def alloc[A: ClassManifest](n: Int): SCLBuffer[_<:java.nio.Buffer] = allocForReadWrite[A](n)
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
		val dataSize = if (args.length > 0) args(0).toInt else 1000

		val src = ("\n" + 
				"__kernel void copyVec(                         \n" +
				"   __global const float* input,                \n" +
				"   __global float* output)                     \n" +
				"{                                              \n" +
				"   int i = get_global_id(0);                   \n" +
				"   output[i] = input[i];                       \n" +
		"}                                              \n")

		// kernel has external and internal interface
		// internal: what local/private memory does it use, etc.?
		// kernel specifies memory effects (but want to HIDE from the programmer
		// interface -- it's just an attribute of the kernel)

		val kernel = CL.gpu.compile1[Array[Float], Array[Float]]("copyVec", src)

		val a = Array.tabulate(dataSize)(_.toFloat)

		val c = time {
			val result = CL.gpu.spawn(dataSize) { kernel(a) }
			result.force
		}
		
//		println("len = " + c.length)
//		for (i <- 0 until c.length)
//			println(i + ": " + c(i))
	}
}


