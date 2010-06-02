package firepile

trait Future[B] { def force: B }
