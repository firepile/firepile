package simplecl

import com.nativelibs4java.opencl._
import java.nio._
import scala.collection.mutable.ArrayBuffer

class SCLContext(clc: CLContext) {
  val _CLContext: CLContext = clc

  def createProgram(srcs: String*): SCLProgram = { 
    new SCLProgram(clc.createProgram(srcs:_*))
  }

  def createProgram(devices: List[SCLDevice], srcs: String*): SCLProgram = {
    val a = new ArrayBuffer[CLDevice]
    devices.foreach(d => a += d._CLDevice)
    new SCLProgram(clc.createProgram(a.toArray, srcs:_*))
  }

  def createDefaultQueue(queueProperties: SCLDeviceUtil.QueueProperties*): SCLQueue = {
    new SCLQueue(_CLContext.createDefaultQueue(queueProperties:_*))
  }

  def createFloatBuffer(kind: CLMem.Usage, count: Long): SCLFloatBuffer = {
    new SCLFloatBuffer(_CLContext.createFloatBuffer(kind, count))
  }

  def createBuffer[B <: Buffer](kind: CLMem.Usage, buffer: B, copy: Boolean): SCLBuffer[B] = {
    new SCLBuffer[B](_CLContext.createBuffer[B](kind, buffer, copy))
  }



  // TODO: This need fixing
  //def createBuffer[A](kind: CLMem.Usage, size: Int, copy: Boolean)(implicit x: scala.reflect.Manifest[A]) = {
  //     x.toString match {
  //       case "Int" => createBuffer[IntBuffer](kind, IntBuffer.allocate(size), copy)
  //       case "Byte" => createBuffer[ByteBuffer](kind, ByteBuffer.allocate(size), copy)
  //       case "Char" => createBuffer[CharBuffer](kind, CharBuffer.allocate(size), copy)
  //       case "Double" => createBuffer[DoubleBuffer](kind, DoubleBuffer.allocate(size), copy)
  //       case "Float" => createBuffer[FloatBuffer](kind, FloatBuffer.allocate(size), copy)
  //       case "Long" => createBuffer[LongBuffer](kind, LongBuffer.allocate(size), copy)
  //       case "Short" => createBuffer[ShortBuffer](kind, ShortBuffer.allocate(size), copy)
  //       case _ => throw new RuntimeException("Buffer type not supported")
  //     }
  //}

  // Added for SimpleCLBuffer, needs to be restricted to possible types somehow
  def createSimpleCLBuffer[B](kind: CLMem.Usage, size: Int, copy: Boolean)(implicit x: scala.reflect.Manifest[B]) = {
       x.toString match {
         case "Int" => new SimpleCLBuffer[Int,IntBuffer](createBuffer[IntBuffer](kind, IntBuffer.allocate(size), copy))
         case "Byte" => new SimpleCLBuffer[Byte,ByteBuffer](createBuffer[ByteBuffer](kind, ByteBuffer.allocate(size), copy))
         case "Char" => new SimpleCLBuffer[Char,CharBuffer](createBuffer[CharBuffer](kind, CharBuffer.allocate(size), copy))
         case "Double" => new SimpleCLBuffer[Double,DoubleBuffer](createBuffer[DoubleBuffer](kind, DoubleBuffer.allocate(size), copy))
         case "Float" => new SimpleCLBuffer[Float,FloatBuffer](createBuffer[FloatBuffer](kind, FloatBuffer.allocate(size), copy))
         case "Long" => new SimpleCLBuffer[Long,LongBuffer](createBuffer[LongBuffer](kind, LongBuffer.allocate(size), copy))
         case "Short" => new SimpleCLBuffer[Short,ShortBuffer](createBuffer[ShortBuffer](kind, ShortBuffer.allocate(size), copy))
         case _ => throw new RuntimeException("Buffer type not supported")
       }
  }
}

