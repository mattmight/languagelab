import net.might.matt.languages.sexp._ ;


object Test {

  def main(args : Array[String]) {

    val fact = SExp.fromFile("tests/fact.scm") 

    println(fact)

  }

}
