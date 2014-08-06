import scala.language.implicitConversions

import net.might.matt.languages.sexp._ ;

import net.might.matt.languages.reg._ ;


object RegSmallStepInterpreter {

  def eval(exp : Exp, env : Map[String,Int]) : Int = {
    exp match {
      case IntExp(n) => n

      case RefExp(v) => env(v)

      case PlusExp(a,b) => eval(a,env) + eval(b,env)

      case TimesExp(a,b) => eval(a,env) * eval(b,env)

      case EqExp(a,b) => if (eval(a,env) == eval(b,env)) { 1 } else { 0 }
    }
  }

  case class State(stmts : List[Stmt], env : Map[String,Int]) {
    val isFinal = false

    def step () : Option[State] = {
      if (stmts.isEmpty) {
        return None
      }

      stmts.head match {
        case LabelStmt(target) => {
          Some(State(stmts.tail, env))
        }

        case AssignStmt(v, exp) => {
          Some(State(stmts.tail, env.updated(v, eval(exp,env))))
        }

        case GotoStmt(target) => {
          Some(State(RegProg.lookup(target),env))
        }

        case IfStmt(exp, target) => {
          val cond = eval(exp,env) 
          if (cond == 0)
            Some(State(stmts.tail, env))
          else
            Some(State(RegProg.lookup(target), env))
        }
      }
    }
  }

  def inject (prog : RegProg) : State = {
    State(prog.stmts, Map())
  }


  def run (prog : RegProg) : State = {
    var last  : Option[State] = None
    var state : Option[State] = Some(inject(prog))

    while (!state.isEmpty) {
      last = state
      state = state.get.step()
    }

    return last.get
  }


  def test() {
    val in = "(+ 42 42)" ;
    
    val sin = SExp.from(in) ;

    val exp = Exp.from(sin) ;

    println(exp) 
    
    println(eval(exp,Map()))

    val src_prog = "((:= x 42) (:= y x))"

    val sexp_prog = SExp.from(src_prog)

    val prog = RegProg.from(sexp_prog)

    println(prog) 
  }
}



