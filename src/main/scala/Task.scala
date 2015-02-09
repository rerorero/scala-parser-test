import scala.util._
import scala.concurrent._
import ExecutionContext.Implicits.global

object Tasks {

  // Result Functor
  trait Result[+A] { 
    def map[B](f: A => B) : Result[B] 
  }
  case class Succeed[+A](result: A) extends Result[A] { 
    def map[B](f: A => B) = Succeed(f(result)) 
  }
  case class Failed(val msg: String) extends Result[Nothing] { 
    def map[B](f:Nothing => B) = this 
  }

  // Readerモナド
  trait Task[S, T] extends (S => Future[T]) { self =>
    def run(in:S) : Result[T]

    def apply(in:S) = future{ 
      asTransactiveAction(in) { _.beginTransaction("session1") }
      run(in) match {
        case Succeed(result) => {
          asTransactiveAction(in) { _.commit("session1") }
          result
        }
        case Failed(e) => {
          asTransactiveAction(in) { _.rollback("session1") }
          throw new Exception(e)
        }
      }
    }

    def asTransactiveAction(in:S)(f: Transactive => Unit) = in match {
      case dao : Transactive => f(dao)
      case _ => println("Not applicative")
    } 

    def map[U](f: T => U) : Task[S, U] = new Task[S, U] { 
      def run(in : S) = self.run(in) map f 
    }

    def flatMap[U](f: T => Task[S, U] ) : Task[S, U] = new Task[S, U] {
      def run(in:S) = self.run(in) match {
        case Succeed(result) => f(result).run(in)
        case Failed(t) => Failed(t)
      }
    }
  }

  // Taskヘルパーメソッド
  def Task[S, T](f: S => T) : Task[S, T] = new Task[S, T] {
    def run(in:S) : Result[T] = Succeed(f(in))
  }

  def TaskIf[S, _](f: S => Boolean) : Task[S, Unit] = new Task[S, Unit] { 
    def run(in:S) : Result[Unit] = if (f(in)) Succeed(()) else Failed("failed in TaskIf")
  }

  // この辺はmonadトランスフォーマーが使えんのかも？
  def TaskTry[S, T](f: S => Try[T]) : Task[S, T] = new Task[S, T] {
    def run(in:S) : Result[T] = f(in) match {
      case Success(result) => Succeed(result)
      case Failure(e) => Failed(e.getMessage)
    }
  }
}

////////////////////////////////////
////////////////////////////////////
trait Transactive {
  def beginTransaction(name:String) : Unit
  def commit(name:String) : Unit 
  def rollback(name:String) : Unit 
}
class MySql extends Transactive{
  def count(q:String) : Int = { println("MySQL :: count " + q); 10 }
  def insert(q:String) : Try[Int] = Try{ println("MySQL :: insert " + q); 20 }
  def insertFailed(q:String) : Try[Int] = Try{ throw new Exception("MySQL :: insert failed")}
  def beginTransaction(name:String) = println("MySQL :: Begin Transaction " + name)
  def commit(name:String) = println("MySQL :: Commit Transaction " + name)
  def rollback(name:String) = println("MySQL :: Rollback Transaction " + name)
}

object taskmain extends App {
  import Tasks._

  def countTicket(ticket: String) : Task[MySql, Int] = Task[MySql, Int] { dao =>
    dao.count(ticket)
  }

  def insertTicket(ticket: String) : Task[MySql, Int] = TaskTry[MySql, Int] { dao =>
    dao.insert(ticket)
    //dao.insertFailed(ticket)
  }

  def checkTicketCount(num1: Int, num2: Int) = TaskIf[MySql, Unit] { dao =>
    num1 < num2
  }

  val sample = for {
    a <- countTicket("time shift")
    _ <- checkTicketCount(a, 15)
    b <- insertTicket("new time shift ticket")
  } yield "ticket count = " + a + ", inserted #" + b

  val mysql = new MySql

  sample(mysql).onComplete {
    case Success(result) => println("Succeed :: " + result)
    case Failure(msg) => println("Failed :: " + msg)
  }
}

