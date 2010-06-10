package firepile

import firepile.util.BufferBackedArray._

object Implicits {
  class ArrayHasLength[A] extends HasLength[Array[A]] {
    def length(a: Array[A]) = a.length
  }
  implicit def ahl[A] = new ArrayHasLength[A]

  class BBArrayHasLength[A] extends HasLength[BBArray[A]] {
    def length(a: BBArray[A]) = a.length
  }
  implicit def bhl[A] = new BBArrayHasLength[A]
}
