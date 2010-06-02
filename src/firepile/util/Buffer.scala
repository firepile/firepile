package simplecl.util

import java.nio.{Buffer => Buf,
                 ByteBuffer => ByteBuf,
                 ShortBuffer => ShortBuf,
                 CharBuffer => CharBuf,
                 IntBuffer => IntBuf,
                 LongBuffer => LongBuf,
                 FloatBuffer => FloatBuf,
                 DoubleBuffer => DoubleBuf,
                 ByteOrder}
import scala.reflect.ClassManifest
import scala.reflect.Manifest

object Buffer {
  def make[T: ClassManifest](n: Int): Buffer[T] = {
      implicitly[ClassManifest[T]] match {
        case m if m == Manifest.Byte => fromNIOBuffer(ByteBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Char => fromNIOBuffer(CharBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Float => fromNIOBuffer(FloatBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Short => fromNIOBuffer(ShortBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Int => fromNIOBuffer(IntBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Long => fromNIOBuffer(LongBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Double => fromNIOBuffer(DoubleBuf.allocate(n)).asInstanceOf[Buffer[T]]
        case _ => throw new MatchError
      }
  }

  def fromArray[T: ClassManifest](a: Array[T]): Buffer[T] = {
      implicitly[ClassManifest[T]] match {
        case m if m == Manifest.Byte => fromNIOBuffer(ByteBuf.wrap(a.asInstanceOf[Array[Byte]])).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Short => fromNIOBuffer(ShortBuf.wrap(a.asInstanceOf[Array[Short]])).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Char => fromNIOBuffer(CharBuf.wrap(a.asInstanceOf[Array[Char]])).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Int => fromNIOBuffer(IntBuf.wrap(a.asInstanceOf[Array[Int]])).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Long => fromNIOBuffer(LongBuf.wrap(a.asInstanceOf[Array[Long]])).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Float => fromNIOBuffer(FloatBuf.wrap(a.asInstanceOf[Array[Float]])).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Double => fromNIOBuffer(DoubleBuf.wrap(a.asInstanceOf[Array[Double]])).asInstanceOf[Buffer[T]]
        case _ => throw new MatchError
      }
  }

  // def makeDirect(n: Int) = fromNIOBuffer[Byte](ByteBuf.allocateDirect(n))
  def makeDirect[T: ClassManifest](n: Int): Buffer[T] = {
      implicitly[ClassManifest[T]] match {
        case m if m == Manifest.Byte => fromNIOBuffer(ByteBuf.allocateDirect(n).order(ByteOrder.nativeOrder)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Char => fromNIOBuffer((ByteBuf.allocateDirect(n * 2).order(ByteOrder.nativeOrder)).asCharBuffer).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Float => fromNIOBuffer((ByteBuf.allocateDirect(n * 4).order(ByteOrder.nativeOrder)).asFloatBuffer).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Short => fromNIOBuffer((ByteBuf.allocateDirect(n * 2).order(ByteOrder.nativeOrder)).asShortBuffer).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Int => fromNIOBuffer((ByteBuf.allocateDirect(n * 4).order(ByteOrder.nativeOrder)).asIntBuffer).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Long => fromNIOBuffer((ByteBuf.allocateDirect(n * 8).order(ByteOrder.nativeOrder)).asLongBuffer).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Double => fromNIOBuffer((ByteBuf.allocateDirect(n * 8).order(ByteOrder.nativeOrder)).asDoubleBuffer).asInstanceOf[Buffer[T]]
        case _ => throw new MatchError
      }
  }

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
        case m if m == Manifest.Short => fromSeq(xs.asInstanceOf[Seq[Short]], ShortBuf.allocate(_), _.asInstanceOf[ShortBuf].put(_: Short)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Char => xs match {
            case xs : IndexedSeq[_] => fromNIOBuffer(CharBuf.wrap(seqToCharSequence(xs.asInstanceOf[IndexedSeq[Char]]))).asInstanceOf[Buffer[T]]
            case xs => fromSeq(xs.asInstanceOf[Seq[Char]], CharBuf.allocate(_), _.asInstanceOf[CharBuf].put(_: Char)).asInstanceOf[Buffer[T]]
        }
        case m if m == Manifest.Int => fromSeq(xs.asInstanceOf[Seq[Int]], IntBuf.allocate(_), _.asInstanceOf[IntBuf].put(_: Int)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Long => fromSeq(xs.asInstanceOf[Seq[Long]], LongBuf.allocate(_), _.asInstanceOf[LongBuf].put(_: Long)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Float => fromSeq(xs.asInstanceOf[Seq[Float]], FloatBuf.allocate(_), _.asInstanceOf[FloatBuf].put(_: Float)).asInstanceOf[Buffer[T]]
        case m if m == Manifest.Double => fromSeq(xs.asInstanceOf[Seq[Double]], DoubleBuf.allocate(_), _.asInstanceOf[DoubleBuf].put(_: Double)).asInstanceOf[Buffer[T]]

        case _ => throw new MatchError
      }
  }

  // def make(a: Seq[Char], start: Int, end: Int): Buffer[Char] = CharBuf.wrap(a, start, end)

  def fromNIOBuffer(b: ByteBuf): Buffer[Byte] = new WrappedByteBuffer(b)
  def fromNIOBuffer(b: ShortBuf): Buffer[Short] = new WrappedShortBuffer(b)
  def fromNIOBuffer(b: CharBuf): Buffer[Char] = new WrappedCharBuffer(b)
  def fromNIOBuffer(b: IntBuf): Buffer[Int] = new WrappedIntBuffer(b)
  def fromNIOBuffer(b: LongBuf): Buffer[Long] = new WrappedLongBuffer(b)
  def fromNIOBuffer(b: FloatBuf): Buffer[Float] = new WrappedFloatBuffer(b)
  def fromNIOBuffer(b: DoubleBuf): Buffer[Double] = new WrappedDoubleBuffer(b)

  def fromNIOBuffer[T: ClassManifest](b: Buf): Buffer[T] = b match {
    case b : ByteBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
    case b : ShortBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
    case b : CharBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
    case b : IntBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
    case b : LongBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
    case b : FloatBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
    case b : DoubleBuf => fromNIOBuffer(b).asInstanceOf[Buffer[T]]
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
      def put(a: Int, b: Byte) = wrap(buf.put(a, b))
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
      def put(a: Int, b: Char) = wrap(buf.put(a, b))
      def putArray(src: Array[Char]) = wrap(buf.put(src))
      def putArray(src: Array[Char], offset: Int, length: Int) = wrap(buf.put(src, offset, length))
      // putChar ...
      def slice = wrap(buf.slice)

      // compareTo
      // equals
      // hashCode

      override def unwrap: CharBuf = buf
  }

  class WrappedFloatBuffer(buf: FloatBuf) extends WrappedBuffer[Float, FloatBuf](buf)
  {
      def wrap(otherBuf: FloatBuf) = new WrappedFloatBuffer(otherBuf)

      def asArray: Array[Float] = buf.array
      def arrayOffset: Int = buf.arrayOffset

      def asBuffer[U: ClassManifest]: Buffer[U] = throw new RuntimeException("unimplemented")

      def asReadOnlyBuffer = wrap(buf.asReadOnlyBuffer)
      def compact = wrap(buf.compact)
      def duplicate = wrap(buf.duplicate)

      def get: Float = buf.get
      def get(dst: Array[Float]) = wrap(buf.get(dst))
      def get(dst: Array[Float], offset: Int, length: Int) = wrap(buf.get(dst, offset, length))
      def get(index: Int) = buf.get(index)

      def hasArray = buf.hasArray
      def isDirect = buf.isDirect
      def order: java.nio.ByteOrder = buf.order
      def put(b: Float) = wrap(buf.put(b))
      def put(a: Int, b: Float) = wrap(buf.put(a, b))
      def putArray(src: Array[Float]) = wrap(buf.put(src))
      def putArray(src: Array[Float], offset: Int, length: Int) = wrap(buf.put(src, offset, length))

      def slice = wrap(buf.slice)

      // compareTo
      // equals
      // hashCode

      override def unwrap: FloatBuf = buf
  }

  class WrappedShortBuffer(buf: ShortBuf) extends WrappedBuffer[Short, ShortBuf](buf)
  {
      def wrap(otherBuf: ShortBuf) = new WrappedShortBuffer(otherBuf)

      def asArray: Array[Short] = buf.array
      def arrayOffset: Int = buf.arrayOffset

      def asBuffer[U: ClassManifest]: Buffer[U] = throw new RuntimeException("unimplemented")

      def asReadOnlyBuffer = wrap(buf.asReadOnlyBuffer)
      def compact = wrap(buf.compact)
      def duplicate = wrap(buf.duplicate)

      def get: Short = buf.get
      def get(dst: Array[Short]) = wrap(buf.get(dst))
      def get(dst: Array[Short], offset: Int, length: Int) = wrap(buf.get(dst, offset, length))
      def get(index: Int) = buf.get(index)

      def hasArray = buf.hasArray
      def isDirect = buf.isDirect
      def order: java.nio.ByteOrder = buf.order
      def put(b: Short) = wrap(buf.put(b))
      def put(a: Int, b: Short) = wrap(buf.put(a, b))
      def putArray(src: Array[Short]) = wrap(buf.put(src))
      def putArray(src: Array[Short], offset: Int, length: Int) = wrap(buf.put(src, offset, length))

      def slice = wrap(buf.slice)

      // compareTo
      // equals
      // hashCode

      override def unwrap: ShortBuf = buf
  }

  class WrappedIntBuffer(buf: IntBuf) extends WrappedBuffer[Int, IntBuf](buf)
  {
      def wrap(otherBuf: IntBuf) = new WrappedIntBuffer(otherBuf)

      def asArray: Array[Int] = buf.array
      def arrayOffset: Int = buf.arrayOffset

      def asBuffer[U: ClassManifest]: Buffer[U] = throw new RuntimeException("unimplemented")

      def asReadOnlyBuffer = wrap(buf.asReadOnlyBuffer)
      def compact = wrap(buf.compact)
      def duplicate = wrap(buf.duplicate)

      def get: Int = buf.get
      def get(dst: Array[Int]) = wrap(buf.get(dst))
      def get(dst: Array[Int], offset: Int, length: Int) = wrap(buf.get(dst, offset, length))
      def get(index: Int) = buf.get(index)

      def hasArray = buf.hasArray
      def isDirect = buf.isDirect
      def order: java.nio.ByteOrder = buf.order
      def put(b: Int) = wrap(buf.put(b))
      def put(a: Int, b: Int) = wrap(buf.put(a, b))
      def putArray(src: Array[Int]) = wrap(buf.put(src))
      def putArray(src: Array[Int], offset: Int, length: Int) = wrap(buf.put(src, offset, length))

      def slice = wrap(buf.slice)

      // compareTo
      // equals
      // hashCode

      override def unwrap: IntBuf = buf
  }

  class WrappedLongBuffer(buf: LongBuf) extends WrappedBuffer[Long, LongBuf](buf)
  {
      def wrap(otherBuf: LongBuf) = new WrappedLongBuffer(otherBuf)

      def asArray: Array[Long] = buf.array
      def arrayOffset: Int = buf.arrayOffset

      def asBuffer[U: ClassManifest]: Buffer[U] = throw new RuntimeException("unimplemented")

      def asReadOnlyBuffer = wrap(buf.asReadOnlyBuffer)
      def compact = wrap(buf.compact)
      def duplicate = wrap(buf.duplicate)

      def get: Long = buf.get
      def get(dst: Array[Long]) = wrap(buf.get(dst))
      def get(dst: Array[Long], offset: Int, length: Int) = wrap(buf.get(dst, offset, length))
      def get(index: Int) = buf.get(index)

      def hasArray = buf.hasArray
      def isDirect = buf.isDirect
      def order: java.nio.ByteOrder = buf.order
      def put(b: Long) = wrap(buf.put(b))
      def put(a: Int, b: Long) = wrap(buf.put(a, b))
      def putArray(src: Array[Long]) = wrap(buf.put(src))
      def putArray(src: Array[Long], offset: Int, length: Int) = wrap(buf.put(src, offset, length))

      def slice = wrap(buf.slice)

      // compareTo
      // equals
      // hashCode

      override def unwrap: LongBuf = buf
  }

  class WrappedDoubleBuffer(buf: DoubleBuf) extends WrappedBuffer[Double, DoubleBuf](buf)
  {
      def wrap(otherBuf: DoubleBuf) = new WrappedDoubleBuffer(otherBuf)

      def asArray: Array[Double] = buf.array
      def arrayOffset: Int = buf.arrayOffset

      def asBuffer[U: ClassManifest]: Buffer[U] = throw new RuntimeException("unimplemented")

      def asReadOnlyBuffer = wrap(buf.asReadOnlyBuffer)
      def compact = wrap(buf.compact)
      def duplicate = wrap(buf.duplicate)

      def get: Double = buf.get
      def get(dst: Array[Double]) = wrap(buf.get(dst))
      def get(dst: Array[Double], offset: Int, length: Int) = wrap(buf.get(dst, offset, length))
      def get(index: Int) = buf.get(index)

      def hasArray = buf.hasArray
      def isDirect = buf.isDirect
      def order: java.nio.ByteOrder = buf.order
      def put(b: Double) = wrap(buf.put(b))
      def put(a: Int, b: Double) = wrap(buf.put(a, b))
      def putArray(src: Array[Double]) = wrap(buf.put(src))
      def putArray(src: Array[Double], offset: Int, length: Int) = wrap(buf.put(src, offset, length))

      def slice = wrap(buf.slice)

      // compareTo
      // equals
      // hashCode

      override def unwrap: DoubleBuf = buf
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
    def put(a: Int, b: T): Buffer[T]
    def putArray(src: Array[T]): Buffer[T]
    def putArray(src: Array[T], offset: Int, length: Int): Buffer[T]
    // putChar ...
    def slice: Buffer[T]

    def unwrap = buf
}
