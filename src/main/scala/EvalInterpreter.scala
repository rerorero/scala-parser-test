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
  // mix in の型制限、もっと上手い方法ないか
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

  // match caseでやる場合
  def eval2(e:AST[Any]) : FutureReader[R, Any] = e match {
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

      case IfElse(ifElem: IfClause, elElem: Option[ElseClause]) => eval2(ifElem.condition).flatMap { cond =>
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
        (l,r,operation) match {
          case (ll : Double, rr : Double, "<") => (ll < rr)
          case (ll : Double, rr : Double, ">") => (ll > rr)
        }
      }

      case Trace(msg) => eval2(msg).flatMap( m => Action{_.debug(m.toString)})

      case IssueTicket(v) => for {
        id <- eval2(v)
        ticket <- Action { _.issueTicket(id.toString) }
      } yield ticket

      case CountTicket(v) => for {
        id <- eval2(v)
        c <- Action { _.countTicket(id.toString) }
      } yield { println(c); c.toDouble }
    }


  // もう少しtype safeにやる場合
  // simpleExprとかの定義の分冗長ではある
  // simpleExprを型別に分けて type safeに

  def simpleExpr(v:AST[Any]) : FutureReader[R, Any] = v match {
    case v : Number => number(v)
    case v : Lit => lit(v)
    case v : BoolVal => boolVal(v)
    case v : Var => varVal(v)
    case v => expr(v)
  }

  def number(v:Number) = Action.id(v.value)

  def lit(v:Lit) = Action.id(v.v)

  def boolVal(v:BoolVal) = Action.id(v.value)

  def varVal(v:Var) = Action { _ =>
    context.getVar(v.name) match {
      case Some(value) => Future.successful(value)
      case None => Future.failed(new NoSuchElementException(v.name))
    }
  }

  def expr(v:AST[Any]) : FutureReader[R, Any] = v match {
    case v : Assign => assign(v)
    case v : Trace => trace(v)
    case _ => Action.id(Future.failed(new Exception("invalid expr")))
  }

  def assign(v:Assign) = for (value <- simpleExpr(v.e)) yield context.putVar(v.name, value)

  def trace(v:Trace) = for {
    m <- simpleExpr(v.e)
    _ <- Action { _.debug(m.toString)}
  } yield ()

  def comparable(v:AST[Any]) = v match {
    case v : Number => number(v)
    case v : CountTicket => count(v)
    case _ => Action.id(Future.failed(new Exception("invalid comparable")))
  }

  def compare(v:Compare) = for {
      l <- comparable(v.left)
      r <- comparable(v.right)
    } yield {
      (l,r,v.op) match {
        case (ll : Double, rr : Double, "<") => (ll < rr)
        case (ll : Double, rr : Double, ">") => (ll > rr)
        case _ => Future.failed(new Exception("imvalid omparable"))
      }
    }

  def count(v:CountTicket) = for {
    id <- eval2(v)
    c <- Action { _.countTicket(id.toString) }
  } yield c.toDouble

  def issue(v:IssueTicket) = for {
    id <- eval2(v)
    ticket <- Action { _.issueTicket(id.toString) }
  } yield ticket

  def block(v:Block) = v.l.foldLeft(Action.pure) { (acc, ast) =>
    acc.flatMap { _ => expr(ast)}
  }
}
