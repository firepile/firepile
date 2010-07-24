object NewArrayAccess {

  def h(a: Int) = {
    val z = new Array[Int](10)
    z(2) = 20
    var g = z(2) 
    z(3) = g + 10
  }
}

