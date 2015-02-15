package mogi

import scala.concurrent._

trait Resource

trait Logger extends Resource {
  def debug(msg:String)(implicit ex:ExecutionContext) : Future[Unit]
}

trait Transactive extends Resource {
  def beginTransaction(implicit ex:ExecutionContext) : Future[Unit]
  def commitTransaction(implicit ex:ExecutionContext) : Future[Unit]
  def rollbackTransaction(implicit ex:ExecutionContext) : Future[Unit]
}

trait Entitlements extends Resource {
  def issueTicket(id:String)(implicit ex:ExecutionContext) : Future[Int]
  def countTicket(id:String)(implicit ex:ExecutionContext) : Future[Int]
}

trait Time extends Resource {
  def nowUTC(implicit ex:ExecutionContext) : Future[Int]
}
