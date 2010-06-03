package firepile

trait Future[B] {
  private var result: Option[B] = None
  private var started: Boolean = false

  final def start: Future[B] = {
    if (! started) {
      started = true
      run
    }
    this
  }

  // Override to start the computation; nonblocking
  protected def run: Unit

  // Override to complete the computation; blocking
  protected def finish: B

  final def forced = result match {
    case None => false
    case _ => true
  }

  final def force: B = {
    start

    result match {
      case None => {
        val r = finish
        result = Some(r)
        r
      }
      case Some(r) => r
    }
  }
}
