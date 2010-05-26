package simplecl.util

object BufferBackedArray {
  import java.nio.ByteBuffer 
  import java.nio.ByteOrder 
  import scala.collection.mutable.ArrayLike
  import scala.collection.mutable.Builder

  // TODO: need two kinds of Marshal, one for primitives and tuples of
  // primitives, one for arrays
  // For the former, can compute size without a witness.
  // For the latter, need the witness.
  //
  // As a consequence:
  // Can't pass an Array[Array[Int]] since the 1-d arrays are not necessarily
  // the same size.
  // But, can pass an Array[Int]
  //
  // BBArray only needs the former type of Marshal
  // Translating for passing to Kernels needs the second type of Marshal.

  def marshal[A: Marshal] = implicitly[Marshal[A]]
  def fixedSizeMarshal[A: FixedSizeMarshal] = implicitly[FixedSizeMarshal[A]]

  trait FixedSizeMarshal[A] extends Marshal[A] {
    def size(a: A): Int = size
    def size: Int

    // Needed to make arrays work avoiding passing around both a Marshal and a ClassManifest.
    def manifest: ClassManifest[A]
  }

  trait Marshal[A] {
    def size(a: A): Int
    def align: Int
    // require(align <= size)

    def put(buf: ByteBuffer, i: Int, x: A): Unit
    def get(buf: ByteBuffer, i: Int): A

    def put(x: A): ByteBuffer = {
      val n = size(x)
      val b = ByteBuffer.allocate(n).order(ByteOrder.nativeOrder)
      put(b, 0, x)
      b
    }
    def get(b: ByteBuffer): A = {
      get(b, 0)
    }
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

  import scala.collection.generic.CanBuildFrom
  /*
  implicit def bbarrayCBFrom[A: FixedSizeMarshal, From] = new CanBuildFrom[From,A,BBArray[A]] {
    def apply: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
    def apply(from: From): Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
  }
  */
  implicit def bbarrayCBFromArray[A: FixedSizeMarshal] = new CanBuildFrom[Array[_],A,BBArray[A]] {
    def apply: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
    def apply(from: Array[_]): Builder[A, BBArray[A]] = new BBArrayBuilder[A](from.length*implicitly[FixedSizeMarshal[A]].size)
  }
  implicit def bbarrayCBFromBBArray[A: FixedSizeMarshal] = new CanBuildFrom[BBArray[_],A,BBArray[A]] {
    def apply: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
    def apply(from: BBArray[_]): Builder[A, BBArray[A]] = new BBArrayBuilder[A](from.length*implicitly[FixedSizeMarshal[A]].size)
  }

  class BBArrayBuilder[A: FixedSizeMarshal](private var b: ByteBuffer) extends Builder[A, BBArray[A]] {
    def this(n: Int) = this(ByteBuffer.allocate(n).order(ByteOrder.nativeOrder))

    override def +=(elem: A) = {
      val pos = b.position
      val m = implicitly[FixedSizeMarshal[A]]

      // Grow!
      // println("cap " + b.capacity)
      // println("limit " + b.limit)
      // println("pos " + b.position)
      // println("rem " + b.remaining)

      if (b.capacity < pos + m.size) {
        // println("growing at " + pos + " to " + (b.capacity + m.size) * 2)
        val nb = ByteBuffer.allocate(b.capacity*2)
        b = nb.put(b.rewind.asInstanceOf[ByteBuffer])
      }

      m.put(b, pos, elem)
      b = b.position(pos+m.size).asInstanceOf[ByteBuffer]
      // b = b.limit(pos+m.size).asInstanceOf[ByteBuffer]
      this
    }

    def clear: Unit = { b = b.rewind.asInstanceOf[ByteBuffer] }

    def result = {
      new BBArray[A](b.flip.asInstanceOf[ByteBuffer])
    }
  }

  object BBArray {
    def fromArray[A: FixedSizeMarshal](a: Array[A]) = {
      val cbf = implicitly[CanBuildFrom[Array[A], A, BBArray[A]]]
      var bb = cbf(a)
      for (ai <- a) bb += ai
      bb.result
    }
    def tabulate[A: FixedSizeMarshal](n: Int)(f: Int => A) = {
      val b = new BBArray[A](n)
      for (i <- 0 until n) {
        b(i) = f(i)
      }
      b
    }
    def ofDim[A: FixedSizeMarshal](n: Int) = new BBArray[A](n)
  }

  class BBArray[A: FixedSizeMarshal](private val buf: ByteBuffer) extends ArrayLike[A, BBArray[A]] {
    def this(n: Int) = this(ByteBuffer.allocate(n * implicitly[FixedSizeMarshal[A]].size).order(ByteOrder.nativeOrder)) // ByteBuffer.wrap(Array.ofDim[Byte](n * implicitly[FixedSizeMarshal[A]].size)))

    def buffer: ByteBuffer = buf

    // println("created array with buffer of " + buf.limit + " bytes")

    override def newBuilder: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*marshal.size)

    lazy val marshal = implicitly[FixedSizeMarshal[A]]

    def apply(i: Int): A = marshal.get(buf, i*marshal.size)
    def update(i: Int, x: A): Unit = marshal.put(buf, i*marshal.size, x)
    def length: Int = buf.limit / marshal.size
  }

  def time[T](key: String)(body: => T): T = {
    val t0 = System.currentTimeMillis
    try {
      body
    }
    finally {
      val t1 = System.currentTimeMillis
      println(key + " " + (t1-t0)/1000.)
    }
  }

  def main(args: Array[String]) {
    val n = 100000
    val a = Array.tabulate[(Float,Double,Boolean)](n)(i => (i.toFloat,i.toDouble,i % 2 == 0))
    val b = new BBArray[(Float,Double,Boolean)](n)
    for (i <- 0 until n)
      b(i) = a(i)

    for (i <- 0 until n) {
      if (a(i) != b(i))
        println("fail at " + i + ": " + a(i) + " != " + b(i))
    }

    var y = 0.
    time("a.sum") {
      for (i <- 0 until 1000000) {
        if (a(i % n)._3) {
          y += a(i % n)._1
          y += a(i % n)._2
        }
      }
    }
    println(y)

    var x = 0.
    time("b.sum") {
      for (i <- 0 until 1000000) {
        if (b(i % n)._3) {
          x += b(i % n)._1
          x += b(i % n)._2
        }
      }
    }

    val ma = time("a.map") {
      a.map(t => t._1 + t._2)
    }
    // println(ma)

    val ra = time("a.reduce") {
      ma.reduceLeft[Double](_ + _)
    }
    println(ra)

    val mb = time("b.map") {
      b.map(t => t._1 + t._2)
    }
    // println(mb)

    val rb = time("b.reduce") {
      // b.reduceLeft[(Float,Double)]((t,u) => (t._1 + u._1, t._2 + u._2))
      mb.reduceLeft[Double](_ + _)
    }
    println(rb)

  }
}

