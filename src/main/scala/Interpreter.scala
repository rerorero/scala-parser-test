package mogi

trait Interpreter {
  def eval2[R <: Resource](e: AST[Any]) : FutureReader[R, Any]
}


