import scala.language.implicitConversions

import net.might.matt.languages.sexp._ ;

import net.might.matt.languages.reg._ ;


object RegSmallStepAnalysis {

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
    

 
  def aeval(exp : Exp, aenv : Map[String,AInt]) : AInt = {
     exp match {
       case IntExp(n) => α(n)

       case RefExp(v) => aenv(v)

       case PlusExp(a,b) => aeval(a,aenv) + aeval(b,aenv) 

       // Clearly, handling of these two could be more precise:
       case TimesExp(a,b) => Set(Neg,Zero,Pos) 

       case EqExp(a,b) => Set(Neg,Zero,Pos)

     }
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
    
    println(aeval(exp,Map()))

    val src_prog = "((:= x 42) (:= y x))"

    val sexp_prog = SExp.from(src_prog)

    val prog = RegProg.from(sexp_prog)

    println(prog) 

    val astates = arun(prog)

    println(astates)
  }
}



