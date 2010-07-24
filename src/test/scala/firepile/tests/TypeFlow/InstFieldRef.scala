class MyClass {
  var num = 0;
  val num2 = 0;
}

object InstFieldRef {
  val mc = new MyClass()
  def h(a: Int) = {
    var g = mc.num;
    g
  }
}

