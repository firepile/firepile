package firepile

import firepile.Marshaling._
import firepile.util.BufferBackedArray._

import java.nio.ByteBuffer

class ArrayMarshal[A](marshal: FixedSizeMarshal[A], manifest: ClassManifest[A]) extends Marshal[Array[A]] {
  def sizes(a: Array[A]) = sizes(a.length)
  def sizes(len: Int) = (marshal.size * len) :: Nil
  def align = marshal.align
  override def toBuffer(a: Array[A]) = {
    implicit val m = marshal // need evidence
    implicit val M = manifest // need evidence
    val x = BBArray.fromArray[A](a)
    x.buffer :: Nil
  }
  override def fromBuffer(b: List[ByteBuffer]) = {
    assert(b.length == 1)
    implicit val m = marshal // need evidence
    implicit val M = manifest // need evidence
    val x = new BBArray[A](b.head)
    x.toArray
  }
}
