import net.might.matt.languages.cps._ ;


object CPSSmallStepInterpreter {

  type Addr = Int

  type Env = Map[String,Addr]

  type Store = Map[Addr, D]

  abstract class Value {
    def toInt : Int
    def toBoolean : Boolean
    def isFalse : Boolean
  }

  case class Closure(lam : AExp, env : Env) extends Value {
    def toInt = throw new Exception("not an integer")
    def toBoolean = throw new Exception("not a boolean")
    def isFalse = false
  }
  case class IntValue(value : Int) extends Value {
    val toInt = value
    def toBoolean = throw new Exception("not a boolean")
    def isFalse = false
  }
  case class BooleanValue(value : Boolean) extends Value {
    def toInt = throw new Exception("not an integer")
    def toBoolean = value
    def isFalse = !value
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

  def evalOp (op : PrimOp, args : List[Value]) : Value = op match {
    case SumOp => IntValue(args(0).toInt + args(1).toInt)
    case EqOp => BooleanValue(args(0).toInt == args(1).toInt)
  }

  case class State(cexp : CExp, env : Env, store : Store) {

    def step () : Option[State] = {
      cexp match {
        case HaltExp(exit) => None

        case AppPrimExp(CPSPrim(op),List(a,b,k)) => {
          val cont = eval(k,env,store)

          cont match {
            case Closure(LambdaExp(List(v),body),env2) => {
              val aval : Value = eval(a,env,store)
              val bval : Value = eval(b,env,store)
              val retval = evalOp(op, List(aval, bval))

              val addr = alloc (v)

              val env3 = env2.updated(v,addr)

              val store2 = store.updated(addr,retval)

              Some(State(body, env3, store2))
            }
          }
        }

        case IfExp(cond,body1,body2) => {
          val condValue = eval(cond,env,store)
          if (condValue.isFalse)
            Some(State(body2, env, store))
          else
            Some(State(body1, env, store))
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
