package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._

import java.nio.ByteBuffer

class BBArrayMarshal[A: FixedSizeMarshal] extends Marshal[BBArray[A]] {
  def sizes(a: BBArray[A]) = sizes(a.length)
  def sizes(len: Int) = (fixedSizeMarshal[A].size * len) :: Nil
  def align = fixedSizeMarshal[A].align
  override def toBuffer(a: BBArray[A]) = a.buffer :: Nil
  override def fromBuffer(b: List[ByteBuffer]) = new BBArray(b.head)
}
