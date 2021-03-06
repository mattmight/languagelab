import net.might.matt.languages.cps._ ;

object CPSSmallStepAnalysis {

  class AAddr
  case object UniAAddr extends AAddr
  case class VarAAddr(val v : String) extends AAddr // for 0CFA

  abstract class AValue

  type AD = Set[AValue]

  type AEnv = Map[String,AAddr]

  type AStore = Map[AAddr, AD]

  case class AClosure(lam : AExp, env : AEnv) extends AValue
  case object AIntValue extends AValue
  case class ABooleanValue(value : Boolean) extends AValue

  def aeval(aexp : AExp, aenv : AEnv, astore : AStore) : AD = aexp match {
    case RefExp(v) => astore(aenv(v))
    case LambdaExp(vars,body) => Set(AClosure(aexp, aenv))
    case IntExp(n) => Set(AIntValue)
    case BooleanExp(b) => Set(ABooleanValue(b))
  }

  def joinWith(astore : AStore, aaddr : AAddr, ad : AD) : AStore = {
    (astore get aaddr) match {
      case Some(ad2) => astore.updated(aaddr, ad ++ ad2)
      case None => astore.updated(aaddr, ad)
    }
  }

  // A "univariant" allocator:
  // def aalloc (a : Any) : AAddr = { UniAAddr }

  // A monovariant allocator (0CFA): 
  def aalloc (v : String) = VarAAddr(v)

  case class AState(cexp : CExp, aenv : AEnv, astore : AStore) {

    def step() : List[AState] = {
      cexp match {

        case HaltExp(exit) => List()

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
                  astore2 = joinWith(astore2,a,value)
                }

                AState(body, aenv3, astore2)
              }
            }
          }
        }
      }
    }
  }

  def ainject(prog : CPSProg) : AState = AState(prog.cexp, Map(), Map())

  def arun (prog : CPSProg) : Set[AState] = {
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

    return seen
  }
 
}


