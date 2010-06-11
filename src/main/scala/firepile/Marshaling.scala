package firepile

import java.nio.ByteBuffer 
import java.nio.ByteOrder 

object Marshaling {
  def marshal[A: Marshal] = implicitly[Marshal[A]]

  trait Marshal[A] {
    def sizes(a: A): List[Int]
    def sizes(len: Int): List[Int]
    def align: Int
    // require(align <= size)

    def toBuffer(x: A): List[ByteBuffer]
    def fromBuffer(b: List[ByteBuffer]): A
  }

  def fixedSizeMarshal[A: FixedSizeMarshal] = implicitly[FixedSizeMarshal[A]]

  trait FixedSizeMarshal[A] {
    def size: Int
    def align: Int

    def put(buf:ByteBuffer, i: Int, x: A): Unit
    def get(buf:ByteBuffer, i: Int): A

    // Needed to make arrays work avoiding passing around both a Marshal and a ClassManifest.
    def manifest: ClassManifest[A]
  }

  implicit object ZM extends FixedSizeMarshal[Boolean] {
    val size = 1
    val align = 1
    def put(buf:ByteBuffer, i: Int, x: Boolean) = buf.put(i, if (x) 1 else 0)
    def get(buf:ByteBuffer, i: Int) = if (buf.get(i) == 0) false else true
    val manifest = Predef.manifest[Boolean]
  }

  implicit object BM extends FixedSizeMarshal[Byte] {
    val size = 1
    val align = 1
    def put(buf:ByteBuffer, i: Int, x: Byte) = buf.put(i, x)
    def get(buf:ByteBuffer, i: Int) = buf.get(i)
    val manifest = Predef.manifest[Byte]
  }

  implicit object SM extends FixedSizeMarshal[Short] {
    val size = 2
    val align = 2
    def put(buf:ByteBuffer, i: Int, x: Short) = buf.putShort(i, x)
    def get(buf:ByteBuffer, i: Int) = buf.getShort(i)
    val manifest = Predef.manifest[Short]
  }

  implicit object CM extends FixedSizeMarshal[Char] {
    val size = 2
    val align = 2
    def put(buf:ByteBuffer, i: Int, x: Char) = buf.putChar(i, x)
    def get(buf:ByteBuffer, i: Int) = buf.getChar(i)
    val manifest = Predef.manifest[Char]
  }

  implicit object IM extends FixedSizeMarshal[Int] {
    val size = 4
    val align = 4
    def put(buf:ByteBuffer, i: Int, x: Int) = buf.putInt(i, x)
    def get(buf:ByteBuffer, i: Int) = buf.getInt(i)
    val manifest = Predef.manifest[Int]
  }

  implicit object LM extends FixedSizeMarshal[Long] {
    val size = 8
    val align = 8
    def put(buf:ByteBuffer, i: Int, x: Long) = buf.putLong(i, x)
    def get(buf:ByteBuffer, i: Int) = buf.getLong(i)
    val manifest = Predef.manifest[Long]
  }

  implicit object FM extends FixedSizeMarshal[Float] {
    val size = 4
    val align = 4
    def put(buf:ByteBuffer, i: Int, x: Float) = buf.putFloat(i, x)
    def get(buf:ByteBuffer, i: Int) = buf.getFloat(i)
    val manifest = Predef.manifest[Float]
  }

  implicit object DM extends FixedSizeMarshal[Double] {
    val size = 8
    val align = 8
    def put(buf:ByteBuffer, i: Int, x: Double) = buf.putDouble(i, x)
    def get(buf:ByteBuffer, i: Int) = buf.getDouble(i)
    val manifest = Predef.manifest[Double]
  }

  implicit def tuple2Marshal[A: FixedSizeMarshal, B:FixedSizeMarshal] = new T2M[A,B]

  class T2M[A: FixedSizeMarshal, B:FixedSizeMarshal] extends FixedSizeMarshal[Tuple2[A,B]] {
    val ma = implicitly[FixedSizeMarshal[A]]
    val mb = implicitly[FixedSizeMarshal[B]]

    lazy val size = (ma.size max mb.align) + mb.size
    lazy val align = ma.align max mb.align

    // println("tuple._1 size = " + ma.size)
    // println("tuple._1 align = " + ma.align)
    // println("tuple._2 size = " + mb.size)
    // println("tuple._2 align = " + mb.align)
    // println("tuple size = " + size)
    // println("tuple align = " + align)

    def put(buf:ByteBuffer, i: Int, x: Tuple2[A,B]) = {
        ma.put(buf, i, x._1)
        mb.put(buf, i + (ma.size max mb.align), x._2)
    }
    def get(buf:ByteBuffer, i: Int) = {
        val fst: A = ma.get(buf, i)
        val snd: B = mb.get(buf, i + (ma.size max mb.align))
        (fst, snd)
    }

    val manifest = classManifest[Tuple2[A,B]]
  }

  implicit def tuple3Marshal[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal] = new T3M[A,B,C]

  class T3M[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal] extends FixedSizeMarshal[Tuple3[A,B,C]] {
    val ma = implicitly[FixedSizeMarshal[A]]
    val mb = implicitly[FixedSizeMarshal[B]]
    val mc = implicitly[FixedSizeMarshal[C]]

    lazy val size = (ma.size max mb.align) + (mb.size max mc.align) + mc.size
    lazy val align = ma.align max mb.align max mc.align

    // println("tuple._1 size = " + ma.size)
    // println("tuple._1 align = " + ma.align)
    // println("tuple._2 size = " + mb.size)
    // println("tuple._2 align = " + mb.align)
    // println("tuple size = " + size)
    // println("tuple align = " + align)

    def put(buf:ByteBuffer, i: Int, x: Tuple3[A,B,C]) = {
        ma.put(buf, i, x._1)
        mb.put(buf, i + (ma.size max mb.align), x._2)
        mc.put(buf, i + (ma.size max mb.align) + (mb.size max mc.align), x._3)
    }
    def get(buf:ByteBuffer, i: Int) = {
        val fst: A = ma.get(buf, i)
        val snd: B = mb.get(buf, i + (ma.size max mb.align))
        val thd: C = mc.get(buf, i + (ma.size max mb.align) + (mb.size max mc.align))
        (fst, snd, thd)
    }

    val manifest = classManifest[Tuple3[A,B,C]]
  }
}
