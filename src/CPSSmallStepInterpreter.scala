import net.might.matt.languages.cps._ ;


object CPSSmallStepInterpreter {

  type Addr = Int

  type Env = Map[String,Addr]

  type Store = Map[Addr, D]

  abstract class Value 

  case class Closure(lam : AExp, env : Env) extends Value

  type D = Value // D means "Denotable value"


  def eval(aexp : AExp, env : Env, store : Store) : D = aexp match {
    case RefExp(v) => store(env(v))
    case LambdaExp(vars,body) => Closure(aexp, env)
  }

  var maxAddr : Int = 0 

  def alloc (a : Any) : Addr = { maxAddr += 1 ; maxAddr }

  case class State(cexp : CExp, env : Env, store : Store) {

    def step () : Option[State] = {
      cexp match {
        case HaltExp(exit) => None

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

  def run (prog : CExp) {
  }
}
