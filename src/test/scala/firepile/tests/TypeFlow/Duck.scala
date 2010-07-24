abstract class CanFly 
case class Bird extends CanFly 
case class Duck extends Bird
case class Finch extends Bird

case class Airplane extends CanFly
case class UFO extends CanFly


class TestDuck {
  def test(b: Bird) = {
    var a: CanFly = null
    b match {
      case x: Duck => a = x
      case x: Finch => a = x
    }

    a
  }
}
