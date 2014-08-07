import net.might.matt.languages.cps._ ;


object CPSSmallStepInterpreter {

  type Addr = Int

  type Env = Map[String,Addr]

  type Store = Map[Addr, D]

  abstract class Value {
    def toInt : Int
  }

  case class Closure(lam : AExp, env : Env) extends Value {
    def toInt = throw new Exception("not an integer")
  }
  case class IntValue(value : Int) extends Value {
    val toInt = value
  }
  case class BooleanValue(value : Boolean) extends Value {
    def toInt = throw new Exception("not an integer")
  }

  type D = Value // D means "Denotable value"

  def eval(aexp : AExp, env : Env, store : Store) : D = aexp match {
    case RefExp(v) => store(env(v))
    case LambdaExp(vars,body) => Closure(aexp, env)
    case IntExp(n) => IntValue(n)
    case BooleanExp(b) => BooleanValue(b)
  }

  var maxAddr : Int = 0 

  def alloc (a : Any) : Addr = { maxAddr += 1 ; maxAddr }

  case class State(cexp : CExp, env : Env, store : Store) {

    def step () : Option[State] = {
      cexp match {
        case HaltExp(exit) => None

        case AppPrimExp(CPSPrim(SumOp),List(a,b,k)) => {
          val cont = eval(k,env,store)

          cont match {
            case Closure(LambdaExp(List(v),body),env2) => {
              val aval : Value = eval(a,env,store)
              val bval : Value = eval(b,env,store)
              val retval = IntValue(aval.toInt + bval.toInt)

              val addr = alloc (v)

              val env3 = env2.updated(v,addr)

              val store2 = store.updated(addr,retval)

              Some(State(body, env3, store2))
            }
          }
        }

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

  def inject(prog : CPSProg) : State = State(prog.cexp, Map(), Map())

 
  def run (prog : CPSProg) : State = {
    var last  : Option[State] = None
    var state : Option[State] = Some(inject(prog))

    while (!state.isEmpty) {
      last = state
      state = state.get.step()
    }

    return last.get
  }

  
  def main(args : Array[String]) {
    test()
  }
  

  def test() {

    import net.might.matt.languages.sexp._ ;

    val test1 = "((lambda (x) (+/cps x 4 (lambda (n) (halt n)))) 3)" 
    val sx1 = SExp.from(test1) 
    val prog1 = CPSProg.from(sx1)

    val fin1 = run(prog1)  
 
    println(fin1) 

  }
}
