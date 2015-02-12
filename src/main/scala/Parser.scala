package mogi

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._

object Parser extends StandardTokenParsers {

  override val lexical = new StdLexical

  def parse(input: String) : Either[String, AST] = phrase(program)(new lexical.Scanner(input)) match {
    case Success(ast, _) => Right(ast)
    case e: NoSuccess => Left("parser error:" + e.msg)
  }

  val functions = ("trace issue count" split ' ')
  val controlStatements = ("if true false" split ' ')

  lexical.reserved ++= functions
  lexical.reserved ++= controlStatements
  lexical.delimiters ++=  ("+ - * / ( ) { } , == = < > <> <= >=" split ' ')

  // parser
  type P[+T] = Parser[T]

  lazy val program      = rep(statement) ^^ (Program(_))

  lazy val statement    = expr | ifElse

  lazy val expr:P[AST] = assign | trace | issue | count | compare

  lazy val trace      = "trace" ~> simpleExpr         ^^ (Trace(_))

  lazy val issue      = "issue" ~> simpleExpr         ^^ (IssueTicket(_))

  lazy val count      = "count" ~> simpleExpr         ^^ (CountTicket(_))

  lazy val assign       = ident ~ ("=" ~> simpleExpr)     ^^ { case i ~ e => Assign(i, e) }

  lazy val compare    = (simpleExpr | count) ~ ("<" | ">") ~ (simpleExpr | count) ^^ { case l ~ op ~ r => Compare(l, r, op) }

  lazy val simpleExpr   = ( ident               ^^ Var
    | numericLit          ^^ { x => IntVal(x.toInt) }
    | stringLit           ^^ Lit
    | "true"              ^^ { _ => Lit(true) }
    | "false"             ^^ { _ => Lit(false) }
    | "(" ~> expr <~ ")"
    | failure ("Expression expected")
    )

  lazy val block        = "{" ~> rep(expr) <~ "}"  ^^ (Block(_))

  lazy val lhs                = simpleExpr | compare
  lazy val ifClause           = "if" ~> "(" ~ lhs ~ ")" ~ (block | expr)  ^^ { case _ ~ ex ~ _ ~ bl => IfClause(ex, bl) }
  lazy val elseClause         = "else" ~ (block | expr)                   ^^ { case _ ~ elem => ElseClause(elem) }
  lazy val ifElse             = ifClause ~ opt(elseClause) ^^ { case e1 ~ e2 => IfElse(e1, e2) }
}
