package mogi

import scala.concurrent._

trait FutureReader[A, +B] extends (A => Future[B]) { self =>
  def map[C](f: B => C)(implicit ex: ExecutionContext) = FutureReader { self((_:A)).map(f)(ex) }

  def flatMap[C](f:B => FutureReader[A, C])(implicit ex: ExecutionContext) = FutureReader { in : A =>
    self(in).flatMap( v => f(v)(in) )(ex)
  }
}

object FutureReader {
 def apply[A, B](f: A => Future[B]) = new FutureReader[A,B] {
    def apply(in:A) = f(in)
  }
  def id[R, T](value:T) = FutureReader[R, T] { _ => Future.successful(value) }
  def pure[R] = id[R, Any](())
}
