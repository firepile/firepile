package simplecl.util

import java.nio.{Buffer => Buf,
                 ByteBuffer => ByteBuf,
                 ShortBuffer => ShortBuf,
                 CharBuffer => CharBuf,
                 IntBuffer => IntBuf,
                 LongBuffer => LongBuf,
                 FloatBuffer => FloatBuf,
                 DoubleBuffer => DoubleBuf}
import scala.reflect.ClassManifest
import scala.reflect.Manifest

object Buffer {
  def make[T: ClassManifest](n: Int): Buffer[T] = {
      implicitly[ClassManifest[T]] match {
        case m if m == Manifest.Byte => fromNIOBuffer(ByteBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Char => fromNIOBuffer(CharBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case _ => throw new MatchError
      }
  }

  def makeDirect(n: Int) = fromNIOBuffer[Byte](ByteBuf.allocateDirect(n))

  // def make[T: ClassManifest](a: Array[T]): Buffer[T] = ByteBuf.wrap(a)
  // def make[T: ClassManifest](a: Array[T], offset: Int, length: Int): Buffer[T] = ByteBuf.wrap(a, offset, length)

  private def fromSeq[T: ClassManifest](xs: Seq[T], makeB: Int => Buf, put: (Buf, T) => Buf): Buffer[T] = fromNIOBuffer({
            val ys = xs
            val n = ys.length
            var b = makeB(n)
            for (y <- ys)
                b = put(b, y)
            b.rewind
          })

  def fromSeq[T: ClassManifest](xs: Seq[T]): Buffer[T] = {
      implicitly[ClassManifest[T]] match {
        case m if m == Manifest.Byte => fromSeq(xs.asInstanceOf[Seq[Byte]], ByteBuf.allocate(_), _.asInstanceOf[ByteBuf].put(_: Byte)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Char => xs match {
            case xs : IndexedSeq[_] => fromNIOBuffer(CharBuf.wrap(seqToCharSequence(xs.asInstanceOf[IndexedSeq[Char]]))).asInstanceOf[Buffer[T]]
            case xs => fromSeq(xs.asInstanceOf[Seq[Char]], CharBuf.allocate(_), _.asInstanceOf[CharBuf].put(_: Char)).asInstanceOf[Buffer[T]]
        }
        case _ => throw new MatchError
      }
  }

  // def make(a: Seq[Char], start: Int, end: Int): Buffer[Char] = CharBuf.wrap(a, start, end)

  def fromNIOBuffer(b: ByteBuf): Buffer[Byte] = new WrappedByteBuffer(b)
  def fromNIOBuffer(b: CharBuf): Buffer[Char] = new WrappedCharBuffer(b)

  def fromNIOBuffer[T: ClassManifest](b: Buf): Buffer[T] = b match {
    case b : ByteBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
    case b : CharBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
    // case b : ShortBuf => new WrappedShortBuffer(b)
    // case b : IntBuf => new WrappedIntBuffer(b)
    // case b : LongBuf => new WrappedLongBuffer(b)
    // case b : FloatBuf => new WrappedFloatBuffer(b)
    // case b : DoubleBuf => new WrappedDoubleBuffer(b)
    case _ => throw new MatchError
  }

  abstract class WrappedBuffer[T: ClassManifest, B <: Buf] protected (buf: B) extends Buffer[T](buf) {
      def clear: Buffer[T] = wrap(buf.clear.asInstanceOf[B])
      def flip: Buffer[T] = wrap(buf.flip.asInstanceOf[B])

      def limit(newLimit: Int): Buffer[T] = wrap(buf.limit(newLimit).asInstanceOf[B])
      def mark: Buffer[T] = wrap(buf.mark.asInstanceOf[B])
      def position(newPosition: Int): Buffer[T] = wrap(buf.position(newPosition).asInstanceOf[B])
      def reset: Buffer[T] = wrap(buf.reset.asInstanceOf[B])
      def rewind: Buffer[T] = wrap(buf.rewind.asInstanceOf[B])

      protected def wrap(buf: B): WrappedBuffer[T,B]
  }

  class WrappedByteBuffer(buf: ByteBuf) extends WrappedBuffer[Byte, ByteBuf](buf)
  {
      def wrap(otherBuf: ByteBuf) = new WrappedByteBuffer(otherBuf)

      def asArray: Array[Byte] = buf.array
      def arrayOffset: Int = buf.arrayOffset

      def asBuffer[U: ClassManifest]: Buffer[U] = throw new RuntimeException("unimplemented")

      def asReadOnlyBuffer = wrap(buf.asReadOnlyBuffer)
      def compact = wrap(buf.compact)
      def duplicate = wrap(buf.duplicate)

      def get: Byte = buf.get
      def get(dst: Array[Byte]) = wrap(buf.get(dst))
      def get(dst: Array[Byte], offset: Int, length: Int) = wrap(buf.get(dst, offset, length))
      def get(index: Int) = buf.get(index)
      // getChar ...
      def hasArray = buf.hasArray
      def isDirect = buf.isDirect
      def order: java.nio.ByteOrder = buf.order
      def put(b: Byte) = wrap(buf.put(b))
      def putArray(src: Array[Byte]) = wrap(buf.put(src))
      def putArray(src: Array[Byte], offset: Int, length: Int) = wrap(buf.put(src, offset, length))
      // putChar ...
      def slice = wrap(buf.slice)

      // compareTo
      // equals
      // hashCode

      override def unwrap: ByteBuf = buf
  }

  class WrappedCharBuffer(buf: CharBuf) extends WrappedBuffer[Char, CharBuf](buf)
  {
      def wrap(otherBuf: CharBuf) = new WrappedCharBuffer(otherBuf)

      def asArray: Array[Char] = buf.array
      def arrayOffset: Int = buf.arrayOffset

      def asBuffer[U: ClassManifest]: Buffer[U] = throw new RuntimeException("unimplemented")

      def asReadOnlyBuffer = wrap(buf.asReadOnlyBuffer)
      def compact = wrap(buf.compact)
      def duplicate = wrap(buf.duplicate)

      def get: Char = buf.get
      def get(dst: Array[Char]) = wrap(buf.get(dst))
      def get(dst: Array[Char], offset: Int, length: Int) = wrap(buf.get(dst, offset, length))
      def get(index: Int) = buf.get(index)
      // getChar ...
      def hasArray = buf.hasArray
      def isDirect = buf.isDirect
      def order: java.nio.ByteOrder = buf.order
      def put(b: Char) = wrap(buf.put(b))
      def putArray(src: Array[Char]) = wrap(buf.put(src))
      def putArray(src: Array[Char], offset: Int, length: Int) = wrap(buf.put(src, offset, length))
      // putChar ...
      def slice = wrap(buf.slice)

      // compareTo
      // equals
      // hashCode

      override def unwrap: CharBuf = buf
  }
}

abstract class Buffer[T: ClassManifest] protected (buf: Buf) {
    def capacity: Int = buf.capacity
    def clear: Buffer[T]
    def flip: Buffer[T]
    def hasRemaining: Boolean = buf.hasRemaining
    def isReadOnly: Boolean = buf.isReadOnly
    def limit: Int = buf.limit
    def limit(newLimit: Int): Buffer[T]
    def mark: Buffer[T]
    def position: Int = buf.position
    def position(newPosition: Int): Buffer[T]
    def remaining: Int = buf.remaining
    def reset: Buffer[T]
    def rewind: Buffer[T]

    def asArray: Array[T]
    def arrayOffset: Int

    def asBuffer[U: ClassManifest]: Buffer[U]

    def asReadOnlyBuffer: Buffer[T]
    def compact: Buffer[T]
    def duplicate: Buffer[T]

    def get: T
    def get(dst: Array[T]): Buffer[T]
    def get(dst: Array[T], offset: Int, length: Int): Buffer[T]
    def get(index: Int): T
    // getChar ...
    def hasArray: Boolean
    def isDirect: Boolean
    def order: java.nio.ByteOrder
    def put(b: T): Buffer[T]
    def putArray(src: Array[T]): Buffer[T]
    def putArray(src: Array[T], offset: Int, length: Int): Buffer[T]
    // putChar ...
    def slice: Buffer[T]

    def unwrap = buf
}
