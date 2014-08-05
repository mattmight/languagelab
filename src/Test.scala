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

 
  def aeval(exp : Exp) : AInt = {
     exp match {
       case IntExp(n) => α(n)

       case PlusExp(a,b) => aeval(a) + aeval(b) 
     }
  }


  abstract class AbstractState {
    def step () : AbstractState
  }

  case class State(stmts : List[Stmt], env : Map[String,Int]) extends AbstractState {
    def step () : AbstractState = {
      if (stmts.isEmpty) {
        return FinalState(env)
      }

      stmts.head match {
        case AssignStmt(v, exp) => {
          State(stmts.tail, env.updated(v, eval(exp,env)))
        }
      }
    }
  }

  case class FinalState(env : Map[String,Int]) extends AbstractState {
    def step () : FinalState = {
      return this ;
    }
  }


  def inject (prog : RegProg) : State = {
    State(prog.stmts, Map())
  }

  

  def test() {
    val in = "(+ 42 42)" ;
    
    val sin = SExp.from(in) ;

    val exp = Exp.from(sin) ;

    println(exp) 
    
    println(eval(exp,Map()))

    println(aeval(exp))

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
