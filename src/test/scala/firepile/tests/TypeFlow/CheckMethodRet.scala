
class MyColl[A] {
  def contains(x: A): Boolean = true
}

object CheckMethodRet {
  val r: Int = 0

  def foo(x: MyColl[Int], i: Int) = {
    val found = x.contains(i)
    found
  }
}
