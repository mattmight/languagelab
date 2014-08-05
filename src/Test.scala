import scala.language.implicitConversions

import net.might.matt.languages.sexp._ ;


object TestCPS {
  import net.might.matt.languages.cps._ ;
}

object TestReg {
  import net.might.matt.languages.reg._ ;


  type AInt = Set[Polarity]

  abstract class Polarity {
    def + (pol : Polarity) : Set[Polarity]
  }
  case object Pos extends Polarity {
    def + (pol : Polarity) : Set[Polarity] = pol match {
      case Pos => Set(Pos)
      case Zero => Set(Pos)
      case Neg => Set(Pos,Neg,Zero)
    }
  }
  case object Neg extends Polarity {
    def + (pol : Polarity) : Set[Polarity] = pol match {
      case Pos => Set(Neg,Zero,Pos)
      case Zero => Set(Neg)
      case Neg => Set(Neg)
    }
  }
  case object Zero extends Polarity {
    def + (pol : Polarity) : Set[Polarity] = Set(pol)
  }

  trait Plussable[A] {
    def + (anum : A) : A 
  }

  implicit def makePlussable (anum1 : Set[Polarity]) : Plussable[Set[Polarity]] = new Plussable[Set[Polarity]] {
    def + (anum2 : Set[Polarity]) : Set[Polarity] = {
      val results = for (x <- anum1; y <- anum2) yield { x + y }
      results.foldLeft (Set[Polarity]()) (_ ++ _)
    }
  }


  def α(n : Int) : AInt = 
    if       (n == 0) { Set[Polarity](Zero) }
    else if  (n <  0) { Set[Polarity](Neg) }
    else if  (n >  0) { Set[Polarity](Pos) }
    else              { throw new RuntimeException("Impossible!") }
    

  def eval(exp : Exp, env : Map[String,Int]) : Int = {
    exp match {
      case IntExp(n) => n

      case RefExp(v) => env(v)

      case PlusExp(a,b) => eval(a,env) + eval(b,env)

      case TimesExp(a,b) => eval(a,env) * eval(b,env)

      case EqExp(a,b) => if (eval(a,env) == eval(b,env)) { 1 } else { 0 }
    }
  }

 
  def aeval(exp : Exp, aenv : Map[String,AInt]) : AInt = {
     exp match {
       case IntExp(n) => α(n)

       case RefExp(v) => aenv(v)

       case PlusExp(a,b) => aeval(a,aenv) + aeval(b,aenv) 
     }
  }


  abstract class BaseState {
    def step () : BaseState
    def isFinal : Boolean
  }

  case class State(stmts : List[Stmt], env : Map[String,Int]) extends BaseState {
    val isFinal = false

    def step () : BaseState = {
      if (stmts.isEmpty) {
        return FinalState(env)
      }

      stmts.head match {
        case LabelStmt(target) => {
          State(stmts.tail, env)
        }

        case AssignStmt(v, exp) => {
          State(stmts.tail, env.updated(v, eval(exp,env)))
        }

        case GotoStmt(target) => {
          State(RegProg.lookup(target),env)
        }

        case IfStmt(exp, target) => {
          val cond = eval(exp,env) 
          if (cond == 0)
            State(stmts.tail, env)
          else
            State(RegProg.lookup(target), env)
        }
      }
    }
  }

  case class FinalState(env : Map[String,Int]) extends BaseState {
    val isFinal = true

    def step () : FinalState = {
      return this ;
    }
  }


  def inject (prog : RegProg) : State = {
    State(prog.stmts, Map())
  }


  def run (prog : RegProg) : BaseState = {
    var state : BaseState = inject(prog) 

    while (!state.isFinal)
      state = state.step()

    return state
  }


  case class AState(stmts : List[Stmt], aenv : Map[String,AInt]) {
    def step () : List[AState] = {
      if (stmts.isEmpty) {
        return List() 
      }

      stmts.head match {
        case LabelStmt(target) => {
          List(AState(stmts.tail, aenv))
        }

        case AssignStmt(v, exp) => {
          List(AState(stmts.tail, aenv.updated(v, aeval(exp,aenv))))
        }

        case GotoStmt(target) => {
          List(AState(RegProg.lookup(target),aenv))
        }

        case IfStmt(exp, target) => {
          List(AState(stmts.tail, aenv), AState(RegProg.lookup(target), aenv))
        }
      }
    }
  }


  def ainject (prog : RegProg) : AState = {
    AState(prog.stmts, Map())
  }





  def test() {
    val in = "(+ 42 42)" ;
    
    val sin = SExp.from(in) ;

    val exp = Exp.from(sin) ;

    println(exp) 
    
    println(eval(exp,Map()))

    println(aeval(exp,Map()))

    val src_prog = "((:= x 42) (:= y x))"

    val sexp_prog = SExp.from(src_prog)

    val prog = RegProg.from(sexp_prog)

    println(prog) 

    val state = inject(prog) 

    println(state)
    println(state.step())
    println(state.step().step())
  }
}


object Test {

  def main(args : Array[String]) {

    // val fact = SExp.fromFile("tests/fact.scm") 

    // println(fact)

    TestReg.test() 
  }

}
