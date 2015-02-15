package mogi

import scala.concurrent._

trait StdIoLogger extends Logger {
  def debug(msg:String)(implicit ex:ExecutionContext) : Future[Unit] = Future { println("Logger::"+msg) }
}

trait EntitlementsDao extends Transactive with Entitlements {
  def issueTicket(id:String)(implicit ex:ExecutionContext) : Future[Int] = Future { println("DAO::IssueTicket "+id); 123 }
  def countTicket(id:String)(implicit ex:ExecutionContext) : Future[Int] = Future { println("DAO::CountTicket "+id); 456 }
  def beginTransaction(implicit ex:ExecutionContext) : Future[Unit] = Future { println("DDA::Begin transaction")}
  def commitTransaction(implicit ex:ExecutionContext) : Future[Unit] = Future { println("DDA::Commit transaction")}
  def rollbackTransaction(implicit ex:ExecutionContext) : Future[Unit] = Future { println("DDA::Rollback transaction")}
}
/*
trait Runner[R <: Resource, I <: Interpreter[R]] {
  def run(resource:R, ast:AST[Any]) : FutureReader[R, Any]
}
*/
class DefaultRunner {
  def run[R <: Resource](resource:R, reader:FutureReader[R, Any]) : Future[Any] = reader(resource)
}


