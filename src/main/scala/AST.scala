package mogi

// よりTypeSafeにするため、戻り値やパラメータは型を絞れるものはAny以外にすること
// ただし型付けは他の具象型のASTと組み合わせられなくなることを意味するので、DSLの言語仕様とうまく調整すること

sealed trait AST[+T]

case class Program(l: Seq[AST[Any]]) extends AST[Any]

case class Assign(name: String, e: AST[Any]) extends AST[Any]
case class Var(name: String) extends AST[Any]

case class Lit(v: Any) extends AST[Any] {
  override def toString = "AST :: " + v match {
    case _:String => "\"" + v + "\""
    case _ => v.toString
  }
}

case class Number(value: Double) extends AST[Double]
case class BoolVal(value: Boolean) extends AST[Boolean]

case class Block(l : Seq[AST[Any]]) extends AST[Any]

case class IfClause(condition: AST[Any], block: AST[Any]) extends AST[Any]
case class ElseClause(block: AST[Any]) extends AST[Any]
case class IfElse(ifElem: IfClause, elElem: Option[ElseClause]) extends AST[Any]

case class Trace(e: AST[Any]) extends AST[Any]
case class IssueTicket(e: AST[Any]) extends AST[Any]
case class CountTicket(e: AST[Any]) extends AST[Double]

case class Compare(left: AST[Double], right: AST[Double], op: String) extends AST[Boolean]
