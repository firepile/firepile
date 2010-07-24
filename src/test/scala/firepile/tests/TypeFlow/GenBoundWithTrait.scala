
class GenBoundWithTrait {
  val r: Int = 0

  def foo[B, A <: Seq[B]](x: A, i: B) = {
    val found = x.contains(i)
    found
  }
}
