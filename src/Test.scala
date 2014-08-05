import net.might.matt.languages.sexp._ ;


object TestCPS {
  import net.might.matt.languages.cps._ ;
}

object TestReg {
  import net.might.matt.languages.reg._ ;
}


object Test {

  def main(args : Array[String]) {
    println("Testing") ;

    val fact = SExp.fromFile("tests/fact.scm") ;

    println(fact)
  }

}
