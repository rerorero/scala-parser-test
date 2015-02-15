package mogi

import scala.concurrent.ExecutionContext


// Contextはミュータブル
class Context() {
  type VarName = String
  type VarValue = Any
  type Environment = collection.immutable.Map[VarName, VarValue]

  var env: Environment = Map()

  override def toString = "Env: " + env.mkString(", ")

  def putVar(name:VarName, value:VarValue) : Unit = {
    println("context: "+name+"="+value)
    env += (name -> value)
  }
  def getVar(name:VarName) : Option[VarValue] = env.get(name)
}

object EvalInterpreter {
  def apply(ast:AST[Any])(implicit ex:ExecutionContext) = (new EvalInterpreter[Logger with Entitlements]).eval2(ast)
}

class EvalInterpreter[R <: Logger with Entitlements](implicit ex : ExecutionContext) {
  import scala.concurrent._

  object Action {
    def apply[T](f:R => Future[T]) = FutureReader(f)
    def id[T](value:T) = FutureReader[R, T] { _ => Future.successful(value) }
    def pure = id[Any](())
  }

  val context = new Context()

  def eval2(e:AST[Any]) : FutureReader[R, Any] = {
    println("eval2:"+e)

    val res = e match {
      case Program(nodes) => nodes.foldLeft(Action.pure) { (acc, ast) =>
        acc.flatMap { _ => eval2(ast)}
      }

      case Block(nodes) => nodes.foldLeft(Action.pure) { (acc, ast) =>
        acc.flatMap { _ => eval2(ast)}
      }

      case Assign(id, node) => for (value <- eval2(node)) yield context.putVar(id, value)

      case Var(id) => Action { _ =>
        context.getVar(id) match {
          case Some(value) => Future.successful(value)
          case None => Future.failed(new NoSuchElementException(id))
        }
      }

      case Lit(v) => Action.id(v)

      case Number(v) => Action.id(v)

      case BoolVal(v) => Action.id(v)

      case IfElse(ifElem: IfClause, elElem: Option[ElseClause]) => for {
        cond <- eval2(ifElem.condition)
      } yield {
        cond match {
          case true => eval2(ifElem.block)
          case false => elElem match {
            case Some(el) => eval2(el)
            case None => Action.pure
          }
        }
      }

      case ElseClause(block) => eval2(block)

      case IfClause(_, _) => Action { _ => Future.failed(new Exception("okashii!!"))}

      case Compare(left, right, operation) => for {
        l <- eval2(left)
        r <- eval2(right)
      } yield {
        (l.asInstanceOf[Double], r.asInstanceOf[Double], operation) match {
          case (ll, rr, "<") => (ll < rr)
          case (ll, rr, ">") => (ll > rr)
        }
      }

      //case Trace(msg) => eval2(msg).flatMap( m => Action { _.debug(m.toString) })
      case Trace(msg) => for {
        m <- eval2(msg)
      } yield Action {
          _.debug(m.toString)
        }

      case IssueTicket(v) => eval2(v).flatMap(id => Action {
        _.issueTicket(id.toString)
      })

      case CountTicket(v) => eval2(v).flatMap(id => Action {
        _.countTicket(id.toString)
      })

    }

    println("ret:"+res)
    res
  }


}
