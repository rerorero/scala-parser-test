package mogi

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

class EvalInterpreter {
  val context = new Context()

  def eval(e: AST[Any]): Any = e match {

    case Program(nodes) => nodes foreach eval

    case Block(nodes) => nodes foreach eval

    case Assign(id, node) => context.putVar(id, eval(node))

    case Var(id) => context.getVar(id).getOrElse(error("Undefined var " + id))

    case Lit(v) => v

    case Number(v) => v

    case BoolVal(v) => v

    case IfElse(ifElem: IfClause,  elElem: Option[ElseClause]) => eval(ifElem.condition) match {
      case true => eval(ifElem.block)
      case false => elElem map { x => eval(x.block)}
      case e => println("unexpected if clause : "+e)
    }

    case Compare(left : AST[Double], right : AST[Double], op) => (eval(left), eval(right)) match {
      case (l : Double, r : Double) => op match {
        case "<" => (l < r)
        case ">" => (l > r)
      }
      case (l,r) => println("Compare error : not number:" + (l, r))
    }

    case Trace(msg) => println("trace::"+eval(msg))

    case IssueTicket(e) => println("issue tickets:"+e)
    case CountTicket(e) => { println("count tickets:"+e); 100d}
  }
}
