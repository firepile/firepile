package firepile

trait InstantiatedKernel[B] {
  def run: Future[B]
}
