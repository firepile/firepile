class MyContainer[T] {
  def contains(a: T) = true
}


class GenBound {
  val r: Int = 0
  val mc = new MyContainer[Int]()

  def foo[A](x: MyContainer[A], i: A) = {
    val found = x.contains(i)
    found
  }

  def fooWithInt = {
    foo[Int](mc, r)
  }

  def fooFound = {
    val found = fooWithInt
    found
  }
}
