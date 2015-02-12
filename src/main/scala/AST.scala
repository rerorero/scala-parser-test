package mogi

sealed trait AST

case class Program(l: Seq[AST]) extends AST

case class Assign(name: String, e: AST) extends AST
case class Var(name: String) extends AST

case class Lit(v: Any) extends AST {
  override def toString = "AST :: " + v match {
    case _:String => "\"" + v + "\""
    case _ => v.toString
  }
}

trait Number extends AST
case class IntVal(value: Int) extends Number

case class IfExpr(e1: AST, e2: AST, e3: AST) extends AST

case class Block(l : Seq[AST]) extends AST

case class IfClause(condition: AST, block: AST) extends AST
case class ElseClause(block: AST) extends AST
case class IfElse(ifElem: IfClause, elElem: Option[ElseClause]) extends AST

case class Trace(e: AST) extends AST
case class IssueTicket(e: AST) extends AST
case class CountTicket(e: AST) extends Number

case class Compare(left: AST, right: AST, op: String) extends AST
