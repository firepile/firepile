package firepile.tests.virtual

sealed class VirtualInvokeA {
  def h(a: Int) = a
}

class VirtualInvokeB extends VirtualInvokeA {
  override def h(a: Int) = a + a
}

class VirtualInvokeC extends VirtualInvokeB {
  var x: Int = 0
  override def h(a: Int) = a + a + a
}

class VirtualInvokeX extends VirtualInvokeA {
  override def h(a: Int) = a * a
}

class VirtualInvokeTest {
  def test(vi: VirtualInvokeA) {
    vi.h(10)
  }
}

