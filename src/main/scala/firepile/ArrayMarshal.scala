package firepile

import firepile.util.BufferBackedArray._

import java.nio.ByteBuffer

  class ArrayMarshal[A](marshal: FixedSizeMarshal[A], manifest: ClassManifest[A]) extends Marshal[Array[A]] {
    def size(a: Array[A]) = {
      if (a.length == 0) 0 else marshal.size(a(0)) * a.length
    }
    def align = marshal.align
    override def put(buf:ByteBuffer, i: Int, x: Array[A]) = {
        var j = 0
        var k = i
        while (j < x.length) {
            marshal.put(buf, i+k, x(j))
            j += 1
            k += marshal.size
        }
    }
    override def get(buf:ByteBuffer, i: Int) = {
      implicit val m = marshal // need evidence
      implicit val M = manifest // need evidence
      val x = new BBArray[A](buf.position(i).asInstanceOf[ByteBuffer])
      x.toArray
    }
    override def put(a: Array[A]) = {
      implicit val m = marshal // need evidence
      implicit val M = manifest // need evidence
      val x = BBArray.fromArray[A](a)
      x.buffer
    }
    override def get(b: ByteBuffer) = {
      implicit val m = marshal // need evidence
      implicit val M = manifest // need evidence
      val x = new BBArray[A](b)
      x.toArray
    }
  }
