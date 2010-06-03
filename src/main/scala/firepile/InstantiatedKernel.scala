package firepile

trait InstantiatedKernel[B] {
  def run(dev: Device): Future[B]
}
