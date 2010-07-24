
class MyColl {
  def contains(x: Int): Boolean = true
}

object CheckMethodRet2 {
  val r: Int = 0

  def foo(x: MyColl, i: Int) = {
    val found = x.contains(i)
    found
  }
}
