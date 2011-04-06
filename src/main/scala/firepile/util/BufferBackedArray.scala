package firepile.util

object BufferBackedArray {
  import java.nio.ByteBuffer 
  import java.nio.ByteOrder 
  import scala.collection.mutable.ArrayLike
  import scala.collection.mutable.Builder
  import firepile.Marshaling._

  def allocBuffer(n: Int) = ByteBuffer.allocate(n).order(ByteOrder.nativeOrder)
  def allocDirectBuffer(n: Int) = ByteBuffer.allocateDirect(n).order(ByteOrder.nativeOrder)

  import scala.collection.generic.CanBuildFrom
  implicit def bbarrayCBFromArray[A: FixedSizeMarshal] = new CanBuildFrom[Array[_],A,BBArray[A]] {
    def apply: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
    def apply(from: Array[_]): Builder[A, BBArray[A]] = new BBArrayBuilder[A](from.length*implicitly[FixedSizeMarshal[A]].size)
  }
  implicit def bbarrayCBFromBBArray[A: FixedSizeMarshal] = new CanBuildFrom[BBArray[_],A,BBArray[A]] {
    def apply: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
    def apply(from: BBArray[_]): Builder[A, BBArray[A]] = new BBArrayBuilder[A](from.length*implicitly[FixedSizeMarshal[A]].size)
  }
  implicit def bbarrayCBFromIndexedSeq[A: FixedSizeMarshal] = new CanBuildFrom[IndexedSeq[_],A,BBArray[A]] {
    def apply: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
    def apply(from: IndexedSeq[_]): Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
  }
  implicit def bbarrayCBFromTraversable[A: FixedSizeMarshal] = new CanBuildFrom[Traversable[_],A,BBArray[A]] {
    def apply: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
    def apply(from: Traversable[_]): Builder[A, BBArray[A]] = new BBArrayBuilder[A](16*implicitly[FixedSizeMarshal[A]].size)
  }

  class BBArrayBuilder[A: FixedSizeMarshal](private var b: ByteBuffer) extends Builder[A, BBArray[A]] {
    def this(n: Int) = this(allocBuffer(n))

    override def +=(elem: A) = {
      val pos = b.position
      val m = implicitly[FixedSizeMarshal[A]]

      if (b.capacity < pos + m.size) {
        // println("growing at " + pos + " to " + (b.capacity + m.size) * 2)
        val nb = allocBuffer(b.capacity*2)
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

  class BBArray[A: FixedSizeMarshal](var buffer: ByteBuffer) extends ArrayLike[A, BBArray[A]] with Iterable[A] {
    def this(n: Int) = this(allocBuffer(n * implicitly[FixedSizeMarshal[A]].size))
      // ByteBuffer.wrap(Array.ofDim[Byte](n * implicitly[FixedSizeMarshal[A]].size)))

    def directCopy =
      if (buffer.isDirect) {
        this
      }
      else {
        val copy = allocDirectBuffer(buffer.limit)
        copy.put(buffer)
        new BBArray[A](copy)
      }

    // println("created array with buffer of " + buffer.limit + " bytes")

    override def newBuilder: Builder[A, BBArray[A]] = new BBArrayBuilder[A](16)

    lazy val marshal = implicitly[FixedSizeMarshal[A]]

    def apply(i: Int): A = marshal.get(buffer, i*marshal.size)
    def update(i: Int, x: A): Unit = marshal.put(buffer, i*marshal.size, x)
    def length: Int = buffer.limit / marshal.size
  }
}

