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

  implicit def MM[A:FixedSizeMarshal] = new Marshal[A] {
    lazy val size = implicitly[FixedSizeMarshal[A]].size
    def sizes(a: A) = size :: Nil
    def sizes(len: Int) = size :: Nil
    def align = implicitly[FixedSizeMarshal[A]].align
    def toBuffer(a: A) = {
      val b = ByteBuffer.allocate(size).order(ByteOrder.nativeOrder)
      implicitly[FixedSizeMarshal[A]].put(b, 0, a)
      b :: Nil
    }
    def fromBuffer(b: List[ByteBuffer]) = implicitly[FixedSizeMarshal[A]].get(b.head, 0)
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

  implicit def tuple4Marshal[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal, D:FixedSizeMarshal] = new T4M[A,B,C,D]

  class T4M[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal, D:FixedSizeMarshal] extends FixedSizeMarshal[Tuple4[A,B,C,D]] {
    val ma = implicitly[FixedSizeMarshal[A]]
    val mb = implicitly[FixedSizeMarshal[B]]
    val mc = implicitly[FixedSizeMarshal[C]]
    val md = implicitly[FixedSizeMarshal[D]]

    lazy val size = (ma.size max mb.align) + (mb.size max mc.align) +
                    (mc.size max md.align) + md.size
    lazy val align = ma.align max mb.align max mc.align max md.align

    def put(buf:ByteBuffer, i: Int, x: Tuple4[A,B,C,D]) = {
        var j = i
        ma.put(buf, j, x._1)
        j += ma.size max mb.align
        mb.put(buf, j, x._2)
        j += mb.size max mc.align
        mc.put(buf, j, x._3)
        j += mc.size max md.align
        md.put(buf, j, x._4)
    }

    def get(buf:ByteBuffer, i: Int) = {
        var j = i
        val fst: A = ma.get(buf, j)
        j += ma.size max mb.align
        val snd: B = mb.get(buf, j)
        j += mb.size max mc.align
        val thd: C = mc.get(buf, j)
        j += mc.size max md.align
        val frh: D = md.get(buf, j)
        (fst, snd, thd, frh)
    }

    val manifest = classManifest[Tuple4[A,B,C,D]]
  }

  implicit def tuple5Marshal[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal, D:FixedSizeMarshal, E:FixedSizeMarshal] = new T5M[A,B,C,D,E]

  class T5M[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal, D:FixedSizeMarshal, E:FixedSizeMarshal] extends FixedSizeMarshal[Tuple5[A,B,C,D,E]] {
    val ma = implicitly[FixedSizeMarshal[A]]
    val mb = implicitly[FixedSizeMarshal[B]]
    val mc = implicitly[FixedSizeMarshal[C]]
    val md = implicitly[FixedSizeMarshal[D]]
    val me = implicitly[FixedSizeMarshal[E]]

    lazy val size = (ma.size max mb.align) + (mb.size max mc.align) +
                    (mc.size max md.align) + (md.size max me.align) +
                    me.size
    lazy val align = ma.align max mb.align max mc.align max md.align max me.align

    def put(buf:ByteBuffer, i: Int, x: Tuple5[A,B,C,D,E]) = {
        var j = i
        ma.put(buf, j, x._1)
        j += ma.size max mb.align
        mb.put(buf, j, x._2)
        j += mb.size max mc.align
        mc.put(buf, j, x._3)
        j += mc.size max md.align
        md.put(buf, j, x._4)
        j += md.size max me.align
        me.put(buf, j, x._5)
    }

    def get(buf:ByteBuffer, i: Int) = {
        var j = i
        val fst: A = ma.get(buf, j)
        j += ma.size max mb.align
        val snd: B = mb.get(buf, j)
        j += mb.size max mc.align
        val thd: C = mc.get(buf, j)
        j += mc.size max md.align
        val frh: D = md.get(buf, j)
        j += md.size max me.align
        val fth: E = me.get(buf, j)
        (fst, snd, thd, frh, fth)
    }

    val manifest = classManifest[Tuple5[A,B,C,D,E]]
  }

  implicit def tuple6Marshal[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal, D:FixedSizeMarshal, E:FixedSizeMarshal, F:FixedSizeMarshal] = new T6M[A,B,C,D,E,F]

  class T6M[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal, D:FixedSizeMarshal, E:FixedSizeMarshal, F:FixedSizeMarshal] extends FixedSizeMarshal[Tuple6[A,B,C,D,E,F]] {
    val ma = implicitly[FixedSizeMarshal[A]]
    val mb = implicitly[FixedSizeMarshal[B]]
    val mc = implicitly[FixedSizeMarshal[C]]
    val md = implicitly[FixedSizeMarshal[D]]
    val me = implicitly[FixedSizeMarshal[E]]
    val mf = implicitly[FixedSizeMarshal[F]]

    lazy val size = (ma.size max mb.align) + (mb.size max mc.align) +
                    (mc.size max md.align) + (md.size max me.align) +
                    (me.size max mf.align) + mf.size
    lazy val align = ma.align max mb.align max mc.align max md.align max me.align max mf.align

    def put(buf:ByteBuffer, i: Int, x: Tuple6[A,B,C,D,E,F]) = {
        var j = i
        ma.put(buf, j, x._1)
        j += ma.size max mb.align
        mb.put(buf, j, x._2)
        j += mb.size max mc.align
        mc.put(buf, j, x._3)
        j += mc.size max md.align
        md.put(buf, j, x._4)
        j += md.size max me.align
        me.put(buf, j, x._5)
        j += me.size max mf.align
        mf.put(buf, j, x._6)
    }

    def get(buf:ByteBuffer, i: Int) = {
        var j = i
        val fst: A = ma.get(buf, j)
        j += ma.size max mb.align
        val snd: B = mb.get(buf, j)
        j += mb.size max mc.align
        val thd: C = mc.get(buf, j)
        j += mc.size max md.align
        val frh: D = md.get(buf, j)
        j += md.size max me.align
        val fth: E = me.get(buf, j)
        j += me.size max mf.align
        val sxh: F = mf.get(buf, j)
        (fst, snd, thd, frh, fth, sxh)
    }

    val manifest = classManifest[Tuple6[A,B,C,D,E,F]]
  }
}
