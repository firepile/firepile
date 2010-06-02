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

  implicit def b2bb(b: Buffer[Byte]) = b.unwrap.asInstanceOf[ByteBuf]
  implicit def b2sb(b: Buffer[Short]) = b.unwrap.asInstanceOf[ShortBuf]
  implicit def b2cb(b: Buffer[Char]) = b.unwrap.asInstanceOf[CharBuf]
  implicit def b2ib(b: Buffer[Int]) = b.unwrap.asInstanceOf[IntBuf]
  implicit def b2lb(b: Buffer[Long]) = b.unwrap.asInstanceOf[LongBuf]
  implicit def b2fb(b: Buffer[Float]) = b.unwrap.asInstanceOf[FloatBuf]
  implicit def b2db(b: Buffer[Double]) = b.unwrap.asInstanceOf[DoubleBuf]

  // TODO: This need fixing
  def createBuffer[T: ClassManifest](kind: CLMem.Usage, size: Int, copy: Boolean) = {
    kind match {
      case CLMem.Usage.Input => implicitly[ClassManifest[T]] match {
         case m if m == Manifest.Byte => new SCLBuffer[ByteBuf](_CLContext.createBuffer[ByteBuf](kind, (Buffer.make[Byte](size)), copy))
         case m if m == Manifest.Short => new SCLBuffer[ShortBuf](_CLContext.createBuffer[ShortBuf](kind, (Buffer.make[Short](size)), copy))
         case m if m == Manifest.Char => new SCLBuffer[CharBuf](_CLContext.createBuffer[CharBuf](kind, (Buffer.make[Char](size)), copy))
         case m if m == Manifest.Int => new SCLBuffer[IntBuf](_CLContext.createBuffer[IntBuf](kind, (Buffer.make[Int](size)), copy))
         case m if m == Manifest.Long => new SCLBuffer[LongBuf](_CLContext.createBuffer[LongBuf](kind, (Buffer.make[Long](size)), copy))
         case m if m == Manifest.Float => new SCLBuffer[FloatBuf](_CLContext.createBuffer[FloatBuf](kind, (Buffer.make[Float](size)), copy))
         case m if m == Manifest.Double => new SCLBuffer[DoubleBuf](_CLContext.createBuffer[DoubleBuf](kind, (Buffer.make[Double](size)), copy))
         case m => throw new RuntimeException("Buffer type not supported " + m)
      }
      case _ => implicitly[ClassManifest[T]] match {
         case m if m == Manifest.Byte => new SCLBuffer[ByteBuf](_CLContext.createBuffer[ByteBuf](kind, (Buffer.makeDirect[Byte](size)), copy))
         case m if m == Manifest.Short => new SCLBuffer[ShortBuf](_CLContext.createBuffer[ShortBuf](kind, (Buffer.makeDirect[Short](size)), copy))
         case m if m == Manifest.Char => new SCLBuffer[CharBuf](_CLContext.createBuffer[CharBuf](kind, (Buffer.makeDirect[Char](size)), copy))
         case m if m == Manifest.Int => new SCLBuffer[IntBuf](_CLContext.createBuffer[IntBuf](kind, (Buffer.makeDirect[Int](size)), copy))
         case m if m == Manifest.Long => new SCLBuffer[LongBuf](_CLContext.createBuffer[LongBuf](kind, (Buffer.makeDirect[Long](size)), copy))
         case m if m == Manifest.Float => new SCLBuffer[FloatBuf](_CLContext.createBuffer[FloatBuf](kind, (Buffer.makeDirect[Float](size)), copy))
         case m if m == Manifest.Double => new SCLBuffer[DoubleBuf](_CLContext.createBuffer[DoubleBuf](kind, (Buffer.makeDirect[Double](size)), copy))
         case m => throw new RuntimeException("Buffer type not supported " + m)
       }
     }
  }

}

