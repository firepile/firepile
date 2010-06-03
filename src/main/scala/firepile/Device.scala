package firepile

import firepile._
import firepile.util.BufferBackedArray._

import java.nio.ByteBuffer

import scala.reflect.Manifest
import scala.collection.mutable.ArraySeq

import com.nativelibs4java.opencl.CLMem
import com.nativelibs4java.opencl.CLDevice
import com.nativelibs4java.opencl.CLEvent
import com.nativelibs4java.opencl.CLKernel.LocalSize

import com.nativelibs4java.opencl._
import java.util.EnumSet

import Firepile.Effect
import Firepile.Effect1
import Firepile.Effect2
import Firepile.Dist
import Firepile.Dist1
import Firepile.Dist2
import Firepile.Kernel1
import Firepile.Kernel2
import Mems._

  object Device {
    object Type extends Enumeration {
      type Type = Value
      val GPU, CPU, Accelerator, Default = Value
    }
    object ExecutionCapability extends Enumeration {
      type ExecutionCapability = Value
      val Kernel, NativeKernel = Value
    }
    object QueueProperties extends Enumeration {
      type QueueProperties = Value
      val OutOfOrder, Profiling = Value
    }
    object MemCacheType extends Enumeration {
      type MemCacheType = Value
      val None, ReadOnly, ReadWrite = Value
    }
  }

  class Device(platform: Platform, cld: CLDevice) extends DeviceLike(platform, cld) {
    private def compileString(name: String, src: String): (Dist,Effect) => BufKernel = {
      val program = context.createProgram(src).build
      val code = program.createKernel(name)
      (d: Dist, e: Effect) => new BufKernel(code, d, e)
    }

    def compile1[A: Marshal, B: Marshal](name: String, src: String, dist: Dist1[A], effect: Effect1[A]) = {
      val transA = implicitly[Marshal[A]];
      val transB = implicitly[Marshal[B]];
      val kernel: (Dist,Effect) => BufKernel = compileString(name, src)
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

    def compile2[A1: Marshal, A2: Marshal, B: Marshal](name: String, src: String, dist: Dist2[A1,A2], effect: Effect2[A1,A2]) = {
      val transA1 = implicitly[Marshal[A1]];
      val transA2 = implicitly[Marshal[A2]];
      val transB = implicitly[Marshal[B]];
      val kernel: (Dist,Effect) => BufKernel = compileString(name, src)
      new Kernel2[A1,A2,B] {
        def apply(a1: A1, a2: A2) = new InstantiatedKernel[B] {
          def run(dev: Device) = {
            val bufIn1: ByteBuffer = transA1.put(a1)
            val bufIn2: ByteBuffer = transA2.put(a2)
            val d: Dist = dist(a1, a2)
            val e: Effect = effect(a1, a2)
            val k = kernel(d, e)
            val f = k(bufIn1, bufIn2).run(dev)
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

    private[firepile] lazy val queue = context.createDefaultQueue()

    lazy val global = new GlobalMem(this)
    lazy val local = new LocalMem(this)

    def spawn[B](k: InstantiatedKernel[B]) = k.run(this)
  }
