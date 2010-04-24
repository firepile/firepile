package simplecl

import com.nativelibs4java.opencl._
import java.nio.{Buffer => Buf,
                 ByteBuffer => ByteBuf,
                 ShortBuffer => ShortBuf,
                 CharBuffer => CharBuf,
                 IntBuffer => IntBuf,
                 LongBuffer => LongBuf,
                 FloatBuffer => FloatBuf,
                 DoubleBuffer => DoubleBuf}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.Manifest
import scala.reflect.ClassManifest
import simplecl.util.Buffer

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

  def createFloatBuffer(kind: CLMem.Usage, count: Int): SCLFloatBuffer = {
    //new SCLFloatBuffer(_CLContext.createFloatBuffer(kind, count))
    new SCLFloatBuffer(createBuffer[Float](kind, count, true).clFloatBuffer)
  }

//  def createBuffer[B <: Buffer](kind: CLMem.Usage, buffer: B, copy: Boolean): SCLBuffer[B] = {
//    new SCLBuffer[B](_CLContext.createBuffer[B](kind, buffer, copy))
//  }


  // TODO: This need fixing
  def createBuffer[T: ClassManifest](kind: CLMem.Usage, size: Int, copy: Boolean) = {
    kind match {
      case CLMem.Usage.Input => implicitly[ClassManifest[T]] match {
         case m if m == Manifest.Float => new SCLBuffer[FloatBuf](_CLContext.createBuffer[FloatBuf](kind, (Buffer.make[Float](size)).unwrap.asInstanceOf[FloatBuf], copy))
                                         // new SCLBuffer[FloatBuf](_CLContext.createBuffer[FloatBuf](kind, FloatBuf.allocate(size), copy))
         case m if m == Manifest.Byte => new SCLBuffer[ByteBuf](_CLContext.createBuffer[ByteBuf](kind, (Buffer.make[Byte](size)).unwrap.asInstanceOf[ByteBuf], copy))
         case m if m == Manifest.Char => new SCLBuffer[CharBuf](_CLContext.createBuffer[CharBuf](kind, (Buffer.make[Char](size)).unwrap.asInstanceOf[CharBuf], copy))
         case m if m == Manifest.Double => new SCLBuffer[DoubleBuf](_CLContext.createBuffer[DoubleBuf](kind, (Buffer.make[Double](size)).unwrap.asInstanceOf[DoubleBuf], copy))
         case m if m == Manifest.Long => new SCLBuffer[LongBuf](_CLContext.createBuffer[LongBuf](kind, (Buffer.make[Long](size)).unwrap.asInstanceOf[LongBuf], copy))
         case m if m == Manifest.Short => new SCLBuffer[ShortBuf](_CLContext.createBuffer[ShortBuf](kind, (Buffer.make[Short](size)).unwrap.asInstanceOf[ShortBuf], copy))
         //case "Byte" => createBuffer[ByteBuffer](kind, ByteBuffer.allocate(size), copy)
         //case "Char" => createBuffer[CharBuffer](kind, CharBuffer.allocate(size), copy)
         //case "Double" => createBuffer[DoubleBuffer](kind, DoubleBuffer.allocate(size), copy)
         //case "Float" => createBuffer[FloatBuffer](kind, FloatBuffer.allocate(size), copy)
         //case "Long" => createBuffer[LongBuffer](kind, LongBuffer.allocate(size), copy)
         //case "Short" => createBuffer[ShortBuffer](kind, ShortBuffer.allocate(size), copy)
         case _ => throw new RuntimeException("Buffer type not supported")
      }
      case _ => implicitly[ClassManifest[T]] match {
         case m if m == Manifest.Float => new SCLBuffer[FloatBuf](_CLContext.createBuffer[FloatBuf](kind, (Buffer.makeDirect[Float](size)).unwrap.asInstanceOf[FloatBuf], copy))
           // new SCLBuffer[FloatBuf](_CLContext.createBuffer[FloatBuf](kind, (Buffer.makeDirect[Float](size)).unwrap.asInstanceOf[FloatBuf], copy))
           // new SCLBuffer[FloatBuf](_CLContext.createBuffer[FloatBuf](kind, FloatBuf.allocate(size), copy))
         case m if m == Manifest.Byte => new SCLBuffer[ByteBuf](_CLContext.createBuffer[ByteBuf](kind, (Buffer.makeDirect[Byte](size)).unwrap.asInstanceOf[ByteBuf], copy))
         case m if m == Manifest.Char => new SCLBuffer[CharBuf](_CLContext.createBuffer[CharBuf](kind, (Buffer.makeDirect[Char](size)).unwrap.asInstanceOf[CharBuf], copy))
         case m if m == Manifest.Double => new SCLBuffer[DoubleBuf](_CLContext.createBuffer[DoubleBuf](kind, (Buffer.makeDirect[Double](size)).unwrap.asInstanceOf[DoubleBuf], copy))
         case m if m == Manifest.Long => new SCLBuffer[LongBuf](_CLContext.createBuffer[LongBuf](kind, (Buffer.makeDirect[Long](size)).unwrap.asInstanceOf[LongBuf], copy))
         case m if m == Manifest.Short => new SCLBuffer[ShortBuf](_CLContext.createBuffer[ShortBuf](kind, (Buffer.makeDirect[Short](size)).unwrap.asInstanceOf[ShortBuf], copy))
         //case "Byte" => createBuffer[ByteBuffer](kind, ByteBuffer.allocate(size), copy)
         //case "Char" => createBuffer[CharBuffer](kind, CharBuffer.allocate(size), copy)
         //case "Double" => createBuffer[DoubleBuffer](kind, DoubleBuffer.allocate(size), copy)
         //case "Float" => createBuffer[FloatBuffer](kind, FloatBuffer.allocate(size), copy)
         //case "Long" => createBuffer[LongBuffer](kind, LongBuffer.allocate(size), copy)
         //case "Short" => createBuffer[ShortBuffer](kind, ShortBuffer.allocate(size), copy)
         case _ => throw new RuntimeException("Buffer type not supported")
       }
     }
  }

}

