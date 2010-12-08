package firepile

import firepile.util.BufferBackedArray.allocBuffer
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
      val b = allocBuffer(size)
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


  // Local memory marshal
  abstract class Local[A: Marshal] {
    // def value: A // called only from gpu
  }

  implicit object AMF extends ArrayMarshal[Float](FM, FM.manifest)


  implicit object LZM extends Local[Boolean] {
    val size = 1
    val align = 1
    // def put(buf:ByteBuffer, i: Int, x: Boolean) = buf.put(i, if (x) 1 else 0)
    // def get(buf:ByteBuffer, i: Int) = if (buf.get(i) == 0) false else true
    val manifest = Predef.manifest[Boolean]
  }

  implicit object LBM extends Local[Byte] {
    val size = 1
    val align = 1
    // def put(buf:ByteBuffer, i: Int, x: Byte) = buf.put(i, x)
    // def get(buf:ByteBuffer, i: Int) = buf.get(i)
    val manifest = Predef.manifest[Byte]
  }

  implicit object LSM extends Local[Short] {
    val size = 2
    val align = 2
    // def put(buf:ByteBuffer, i: Int, x: Short) = buf.putShort(i, x)
    // def get(buf:ByteBuffer, i: Int) = buf.getShort(i)
    val manifest = Predef.manifest[Short]
  }

  implicit object LCM extends Local[Char] {
    val size = 2
    val align = 2
    // def put(buf:ByteBuffer, i: Int, x: Char) = buf.putChar(i, x)
    // def get(buf:ByteBuffer, i: Int) = buf.getChar(i)
    val manifest = Predef.manifest[Char]
  }

  implicit object LIM extends Local[Int] {
    val size = 4
    val align = 4
    // def put(buf:ByteBuffer, i: Int, x: Int) = buf.putInt(i, x)
    // def get(buf:ByteBuffer, i: Int) = buf.getInt(i)
    val manifest = Predef.manifest[Int]
  }

  implicit object LLM extends Local[Long] {
    val size = 8
    val align = 8
    // def put(buf:ByteBuffer, i: Int, x: Long) = buf.putLong(i, x)
    // def get(buf:ByteBuffer, i: Int) = buf.getLong(i)
    val manifest = Predef.manifest[Long]
  }

  implicit object LFM extends Local[Float] {
    val size = 4
    val align = 4
    // def put(buf:ByteBuffer, i: Int, x: Float) = buf.putFloat(i, x)
    // def get(buf:ByteBuffer, i: Int) = buf.getFloat(i)
    val manifest = Predef.manifest[Float]
  }

  implicit object LDM extends Local[Double] {
    val size = 8
    val align = 8
    // def put(buf:ByteBuffer, i: Int, x: Double) = buf.putDouble(i, x)
    // def get(buf:ByteBuffer, i: Int) = buf.getDouble(i)
    val manifest = Predef.manifest[Double]
  }

  // Dummy Array2
  class Array2[A: FixedSizeMarshal]

  // implicit def ArrayMarshal[A: FixedSizeMarshal: Manifest]: ArrayMarshal[A] = new ArrayMarshal[A]

  // class LocalArray[A: FixedSizeMarshal](val value: Array[A])
  // class LocalArray[A: FixedSizeMarshal](length: Int) extends Local[Array[A]]
  // class LocalArray2[A: FixedSizeMarshal](length0: Int, length1: Int) extends Local[Array2[A]]

  val padTuples = true
  def alignment[A](m: FixedSizeMarshal[A]) = if (padTuples) (8 max m.align) else m.align
  def pad[A](m: FixedSizeMarshal[A]) = if (padTuples) 8 else 1

  implicit def tuple2Marshal[A: FixedSizeMarshal, B:FixedSizeMarshal] = new T2M[A,B]

  class T2M[A: FixedSizeMarshal, B:FixedSizeMarshal] extends FixedSizeMarshal[Tuple2[A,B]] {
    val ma = implicitly[FixedSizeMarshal[A]]
    val mb = implicitly[FixedSizeMarshal[B]]

    lazy val size = (ma.size max alignment(mb)) + (mb.size max pad(mb))
    lazy val align = alignment(ma) max alignment(mb)

    def put(buf:ByteBuffer, i: Int, x: Tuple2[A,B]) = {
        ma.put(buf, i, x._1)
        mb.put(buf, i + (ma.size max alignment(mb)), x._2)
    }
    def get(buf:ByteBuffer, i: Int) = {
        val fst: A = ma.get(buf, i)
        val snd: B = mb.get(buf, i + (ma.size max alignment(mb)))
        (fst, snd)
    }

    val manifest = classManifest[Tuple2[A,B]]
  }

  implicit def tuple3Marshal[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal] = new T3M[A,B,C]

  class T3M[A: FixedSizeMarshal, B:FixedSizeMarshal, C:FixedSizeMarshal] extends FixedSizeMarshal[Tuple3[A,B,C]] {
    val ma = implicitly[FixedSizeMarshal[A]]
    val mb = implicitly[FixedSizeMarshal[B]]
    val mc = implicitly[FixedSizeMarshal[C]]

    lazy val size = (ma.size max alignment(mb)) + (mb.size max alignment(mc)) + (mc.size max pad(mc))
    lazy val align = alignment(ma) max alignment(mb) max alignment(mc)

    def put(buf:ByteBuffer, i: Int, x: Tuple3[A,B,C]) = {
        ma.put(buf, i, x._1)
        mb.put(buf, i + (ma.size max alignment(mb)), x._2)
        mc.put(buf, i + (ma.size max alignment(mb)) + (mb.size max alignment(mc)), x._3)
    }
    def get(buf:ByteBuffer, i: Int) = {
        val fst: A = ma.get(buf, i)
        val snd: B = mb.get(buf, i + (ma.size max alignment(mb)))
        val thd: C = mc.get(buf, i + (ma.size max alignment(mb)) + (mb.size max alignment(mc)))
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

    lazy val size = (ma.size max alignment(mb)) + (mb.size max alignment(mc)) +
                    (mc.size max alignment(md)) + (md.size max pad(md))
    lazy val align = alignment(ma) max alignment(mb) max alignment(mc) max alignment(md)

    def put(buf:ByteBuffer, i: Int, x: Tuple4[A,B,C,D]) = {
        var j = i
        ma.put(buf, j, x._1)
        j += ma.size max alignment(mb)
        mb.put(buf, j, x._2)
        j += mb.size max alignment(mc)
        mc.put(buf, j, x._3)
        j += mc.size max alignment(md)
        md.put(buf, j, x._4)
    }

    def get(buf:ByteBuffer, i: Int) = {
        var j = i
        val fst: A = ma.get(buf, j)
        j += ma.size max alignment(mb)
        val snd: B = mb.get(buf, j)
        j += mb.size max alignment(mc)
        val thd: C = mc.get(buf, j)
        j += mc.size max alignment(md)
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

    lazy val size = (ma.size max alignment(mb)) + (mb.size max alignment(mc)) +
                    (mc.size max alignment(md)) + (md.size max alignment(me)) +
                    (me.size max pad(me))
    lazy val align = alignment(ma) max alignment(mb) max alignment(mc) max alignment(md) max alignment(me)

    def put(buf:ByteBuffer, i: Int, x: Tuple5[A,B,C,D,E]) = {
        var j = i
        ma.put(buf, j, x._1)
        j += ma.size max alignment(mb)
        mb.put(buf, j, x._2)
        j += mb.size max alignment(mc)
        mc.put(buf, j, x._3)
        j += mc.size max alignment(md)
        md.put(buf, j, x._4)
        j += md.size max alignment(me)
        me.put(buf, j, x._5)
    }

    def get(buf:ByteBuffer, i: Int) = {
        var j = i
        val fst: A = ma.get(buf, j)
        j += ma.size max alignment(mb)
        val snd: B = mb.get(buf, j)
        j += mb.size max alignment(mc)
        val thd: C = mc.get(buf, j)
        j += mc.size max alignment(md)
        val frh: D = md.get(buf, j)
        j += md.size max alignment(me)
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

    lazy val size = (ma.size max alignment(mb)) + (mb.size max alignment(mc)) +
                    (mc.size max alignment(md)) + (md.size max alignment(me)) +
                    (me.size max alignment(mf)) + (mf.size max pad(mf))
    lazy val align = alignment(ma) max alignment(mb) max alignment(mc) max alignment(md) max alignment(me) max alignment(mf)

    def put(buf:ByteBuffer, i: Int, x: Tuple6[A,B,C,D,E,F]) = {
        var j = i
        ma.put(buf, j, x._1)
        j += ma.size max alignment(mb)
        mb.put(buf, j, x._2)
        j += mb.size max alignment(mc)
        mc.put(buf, j, x._3)
        j += mc.size max alignment(md)
        md.put(buf, j, x._4)
        j += md.size max alignment(me)
        me.put(buf, j, x._5)
        j += me.size max alignment(mf)
        mf.put(buf, j, x._6)
    }

    def get(buf:ByteBuffer, i: Int) = {
        var j = i
        val fst: A = ma.get(buf, j)
        j += ma.size max alignment(mb)
        val snd: B = mb.get(buf, j)
        j += mb.size max alignment(mc)
        val thd: C = mc.get(buf, j)
        j += mc.size max alignment(md)
        val frh: D = md.get(buf, j)
        j += md.size max alignment(me)
        val fth: E = me.get(buf, j)
        j += me.size max alignment(mf)
        val sxh: F = mf.get(buf, j)
        (fst, snd, thd, frh, fth, sxh)
    }

    val manifest = classManifest[Tuple6[A,B,C,D,E,F]]
  }
}
