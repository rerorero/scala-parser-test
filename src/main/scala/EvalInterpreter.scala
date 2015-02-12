package mogi

// Contextはミュータブル
class Context() {
  type VarName = String
  type VarValue = Any
  type Environment = collection.immutable.Map[VarName, VarValue]

  var env: Environment = Map()

  override def toString = "Env: " + env.mkString(", ")

  def putVar(name:VarName, value:VarValue) : Unit = env += (name -> value)
  def getVar(name:VarName) : Option[VarValue] = env.get(name)
}

class EvalInterpreter {
  val context = new Context()

  def eval(e: AST): Any = e match {

    case Program(nodes) => nodes foreach eval

    case Block(nodes) => nodes foreach eval

    case Assign(id, node) => context.putVar(id, eval(node))

    case Var(id) => context.getVar(id).getOrElse(error("Undefined var " + id))

    case Lit(v) => v

    case IntVal(v) => v

    case IfElse(ifElem: IfClause,  elElem: Option[ElseClause]) => {
      val condVal = eval(ifElem.condition)
      if (isInstanceOf[Boolean]) {
        throw new Exception("Boolean condition expected." + condVal)
      }else{
        if (condVal.asInstanceOf[Boolean]) {
          eval(ifElem.block)
        }else{
          elElem map { x => eval(x.block) }
        }
      }
    }

    case Compare(left, right, op) => (left,right) match {
      case (IntVal(l), IntVal(r)) => op match {
        case "<" => (l < r)
        case ">" => (l > r)
      }
    }

    case IssueTicket(e) => println("issue tickets:"+e)
    case CountTicket(e) => { println("count tickets:"+e); 100}
  }
}
