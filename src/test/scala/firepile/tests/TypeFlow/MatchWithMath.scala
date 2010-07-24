object MatchWithMath {
  def h(a: Array[Int]) = {
    var x = 10
    var l = 20.0F
    x match {
      case 10 => { l = l * 2.0F; l = x + 10.0F }
      case 20 => l = x
      case 30 => l = x - 10
      case _ => l = 2
    }
  }
}

