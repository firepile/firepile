object FooWithMethod {
  def fooMethod(a: Float) = a + a
}

class FooObj[A] {
  def objFoo[A](a: Array[A]) = a
}

object NewGenericObj {
  val fo = new FooObj[Double]()

  def returnsBool(z: Boolean) = !z

  def h(a: Array[Int]) = {
    var g: Int = 1;

    FooWithMethod.fooMethod(1.1f)
    returnsBool(true)
    fo.objFoo[Double](Array(1.0))
    if (a(0) > 0)
      g = a(0) - 10
    }
}

