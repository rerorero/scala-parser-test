package mogi

import scala.concurrent._
import scala.util._

trait StdIoLogger extends Logger {
  def debug(msg:String)(implicit ex:ExecutionContext) : Future[Unit] = Future { println("Logger::"+msg) }
}

trait EntitlementsDao extends Entitlements with Transactive {
  def issueTicket(id:String)(implicit ex:ExecutionContext) : Future[Int] = Future { println("DAO::IssueTicket "+id); 123 }
  def countTicket(id:String)(implicit ex:ExecutionContext) : Future[Int] = Future { println("DAO::CountTicket "+id); 456 }
  def beginTransaction : Unit = println("DDA::Begin transaction")
  def commitTransaction : Unit =  println("DDA::Commit transaction")
  def rollbackTransaction : Unit = println("DDA::Rollback transaction")
}

class DefaultRunner {

  def run[R <: Resource](resource:R, reader:FutureReader[R, Any])(implicit ex:ExecutionContext) : Future[Any] = {

    def asTransactive(f:Transactive=>Any) : Any = resource match {
      case res: Transactive => f(res)
      case _ => ()
    }

    for {
      _ <- Future{ asTransactive { _.beginTransaction } }
      res <- reader(resource).andThen {
        case Success(result) => asTransactive { _.commitTransaction }
        case Failure(err) => asTransactive { _.rollbackTransaction }
      }
    } yield { println("run yield = " + res); res }
  }
}

