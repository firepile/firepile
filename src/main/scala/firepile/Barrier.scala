package firepile

trait Barrier {
  def barrier: Unit = throw new RuntimeException("barrier")
}
