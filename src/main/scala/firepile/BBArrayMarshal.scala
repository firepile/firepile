package firepile

import firepile.util.BufferBackedArray._

import java.nio.ByteBuffer

class BBArrayMarshal[A: FixedSizeMarshal] extends Marshal[BBArray[A]] {
  def size(a: BBArray[A]) = a.length * fixedSizeMarshal[A].size
  def align = fixedSizeMarshal[A].align
  override def put(buf:ByteBuffer, i: Int, x: BBArray[A]) = {
      var j = 0
      var k = i
      while (j < x.length) {
          fixedSizeMarshal[A].put(buf, i+k, x(j))
          j += 1
          k += fixedSizeMarshal[A].size
      }
  }
  override def get(buf:ByteBuffer, i: Int) = {
      new BBArray(buf.position(i).asInstanceOf[ByteBuffer])
  }
  override def put(a: BBArray[A]) = a.buffer
  override def get(b: ByteBuffer) = new BBArray(b)
}
