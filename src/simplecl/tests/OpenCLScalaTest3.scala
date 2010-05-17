package simplecl.tests

import simplecl._

import java.nio.FloatBuffer
import simplecl.util.Buffer
import scala.reflect.Manifest
import com.nativelibs4java.opencl.CLMem

object OpenCLScalaTest3 {
	object CL {
		private val platform = SimpleCL.listPlatforms(0)
		private val devices = platform.listAllDevices(true)

		devices.foreach(d => println("Device found: vendor = " + d.vendor + " maxComputeUnits = " + d.maxComputeUnits + " at frequency " + d.maxClockFrequency))

		private val context = platform.createContext(null, devices(0))

		val gpu = new Device(context)
	}

	trait Translator[A] {
		type BaseType // <: ClassManifest[BaseType]
		def manifest: ClassManifest[BaseType]
		def size(a: A): Int
		def align: Int
		def copyIn(dev: Device, a: A): Buffer[BaseType]
		def copyOut(dev: Device, b: Buffer[BaseType]): A
	}

	implicit object FAT extends Translator[Array[Float]] { 
		type BaseType = Float
		def manifest = Manifest.Float

		def size(a: Array[Float]) = 4 * a.length
		def align = 4
		def copyIn(dev: Device, a: Array[Float]) = Buffer.fromSeq[Float](a)
		def copyOut(dev: Device, b: Buffer[Float]) = {
			if (b.hasArray) {
				b.asArray
			}
			else {
				val a = Array.ofDim[Float](b.capacity)
				b.get(a)
				a
			}
		}
	}

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
			println("length = " + length)

			val memIn = dev.global.allocForRead[A](length)
			println("memIn = " + memIn)
			val memOut = dev.global.allocForWrite[B](length)
			println("memOut = " + memOut)

			val writeEvent = memIn.write(dev.queue, input, true)
			println("writeEvent = " + writeEvent)

			// HACK
			code.setArgs(memIn.asInstanceOf[SCLBuffer[java.nio.FloatBuffer]], memOut.asInstanceOf[SCLBuffer[java.nio.FloatBuffer]])

			val runEvent = code.enqueueNDRange(dev.queue, Array(totalNumberOfItems), Array(numberOfItemsPerGroup), writeEvent)
			println("runEvent = " + runEvent)

			val bufOut = Buffer.makeDirect[B](length)
			println("bufOut = " + bufOut)
			val readEvent = memOut.read(dev.queue, bufOut, true, runEvent)
			println("readEvent = " + readEvent)

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

	def main(args: Array[String])  = {
		val dataSize = 1000

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

		val result = CL.gpu.spawn(dataSize) { kernel(a) }

		val c: Array[Float] = result.force

		// Compute absolute and relative average errors wrt Java impl
		var totalAbsError = 0.0
		var totalRelError = 0.0

		for(i <- 0 to dataSize-1) {
			var exp = i * Math.sin(i) + 1
			var res = c(i)
			var d = res - exp

			if(exp != 0.0)
				totalRelError += d / exp
				if(d < 0) totalAbsError += -d else totalAbsError += d
		}

		var avgAbsError = totalAbsError / dataSize
		var avgRelError = totalRelError / dataSize

		println("Average absolute error = " + avgAbsError)
		println("Average relative error = " + avgRelError)

	}
}


