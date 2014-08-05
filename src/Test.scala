import scala.language.implicitConversions

import net.might.matt.languages.sexp._ ;


object TestCPS {
  import net.might.matt.languages.cps._ ;


  type Addr = Int

  type Value = Closure

  type Env = Map[String,Addr]

  type Store = Map[Addr, Value]

  case class Closure(lam : AExp, env : Env)

  def eval(aexp : AExp, env : Env, store : Store) : Value = aexp match {
    case RefExp(v) => store(env(v))
    case LambdaExp(vars,body) => Closure(aexp, env)
  }

  var maxAddr : Int = 0 

  def alloc (a : Any) : Addr = { maxAddr += 1 ; maxAddr }

  case class State(cexp : CExp, env : Env, store : Store) {
    def step() : Option[State] = {
      cexp match {
        case AppExp(f, args) => {

          val clo = eval(f,env,store)

          clo match {
            case Closure(LambdaExp(vars,body), env2) => {

              val argVals = args map (eval(_,env,store))

              val addrs = vars.map(alloc)

              var env3 : Env = env2

              for ((v,a) <- vars zip addrs) {
                env3 = env3.updated(v,a)
              }

              var store2 : Store = store

              for ((a,value) <- addrs zip argVals) {
                store2 = store2.updated(a,value)
              }

              Some(State(body, env3, store2))
            }
          }
        }
      }
    }
  }

  def inject(prog : CExp) : State = State(prog, Map(), Map())
  


  class AAddr

  case object UniAAddr extends AAddr

  case class VarAAddr(val v : String) extends AAddr


  type AValue = Set[AClosure]

  type AEnv = Map[String,AAddr]

  type AStore = Map[AAddr, AValue]

  case class AClosure(lam : AExp, env : AEnv)

  def aeval(aexp : AExp, aenv : AEnv, astore : AStore) : AValue = aexp match {
    case RefExp(v) => astore(aenv(v))
    case LambdaExp(vars,body) => Set(AClosure(aexp, aenv))
  }


  // A "univariant" allocator:
  // def aalloc (a : Any) : AAddr = { UniAAddr }

  // A monovariant allocator (0CFA): 
  def aalloc (v : String) = VarAAddr(v)

  case class AState(cexp : CExp, aenv : AEnv, astore : AStore) {
    def step() : List[AState] = {
      cexp match {
        case AppExp(f, args) => {

          val aclos = aeval(f,aenv,astore)

          for (aclo <- aclos.toList) yield {
            aclo match {
              case AClosure(LambdaExp(vars,body), aenv2) => {
  
                val argVals = args map (aeval(_,aenv,astore))
  
                val aaddrs = vars.map(aalloc)

                var aenv3 : AEnv = aenv2

                for ((v,a) <- vars zip aaddrs) {
                  aenv3 = aenv3.updated(v,a)
                }

                var astore2 : AStore = astore

                for ((a,value) <- aaddrs zip argVals) {
                  astore2 = astore2.updated(a,value)
                }

                AState(body, aenv3, astore2)
              }
            }
          }
        }
      }
    }
  }

  def ainject(prog : CExp) : AState = AState(prog, Map(), Map())



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


  def arun (prog : RegProg) : Set[AState] = {
    var todo = List[AState](ainject(prog))
    var seen = Set[AState]()

    while (!todo.isEmpty) {
      val next = todo.head

      // println(next)

      if (!(seen contains next)) {
         val succs = next.step()
         todo = succs ++ todo.tail
         seen = seen + next
      } 
    }

    seen
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

    val astates = arun(prog)

    println(astates)
  }
}


object Test {

  def main(args : Array[String]) {

    // val fact = SExp.fromFile("tests/fact.scm") 

    // println(fact)

    TestReg.test() 
  }

}
