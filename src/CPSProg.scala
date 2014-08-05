package net.might.matt.languages.cps ;

import net.might.matt.languages.sexp._ ;

abstract class AExp {
}

object AExp {
  def from(sexp : SExp) : AExp = {
    sexp match {

      // References:
      case SSymbol(id) => RefExp(id)

      // Lambda terms:
      case SList(SSymbol("lambda"),params,body) => {
        val vars = params.toList map { case SSymbol(id) => id }
        LambdaExp(vars,CExp.from(body))
      }
       

      // Conditionals:
      case STrue() => BoolExp(true)

      case SFalse() => BoolExp(false)


      // Numerics:
      case SInt(value) => IntExp(value) 
      
    } 
  }
}


abstract class CExp {  
}

object CExp {
  def from(sexp : SExp) : CExp = {
    sexp match {

      case SList(SSymbol("if"),cond,ifTrue,ifFalse) =>
       IfExp(AExp.from(cond), from(ifTrue), from(ifFalse))

      case SList(SSymbol("letrec"),
                SList(SList(SSymbol(fun),lambda)),
                body) =>
       LetRecExp(fun,AExp.from(lambda),from(body))
    

     // Application
     case SCons(fun,args) => 
       AppExp(AExp.from(fun), args.toList map AExp.from)

    } 
  }

}




/* Atomic expressions. */
case class RefExp(val id : String) extends AExp {
  override def toString = id
}

case class BoolExp(val value : Boolean) extends AExp

case class IntExp(val value : Int) extends AExp

case class LambdaExp(val params : List[String], val body : CExp) extends AExp {
  override def toString = 
    "(lambda (" +params.mkString(" ")+ ") " +body+ ")"
}


/* Complex expressions. */
case class AppExp(val fun : AExp, args : List[AExp]) extends CExp {
  override def toString = 
    "(" +(fun :: args).mkString(" ")+ ")"
}

case class IfExp(val cond : AExp, 
                 val ifTrue : CExp, 
                 val ifFalse : CExp) extends CExp


case class LetRecExp(val fun : String,
                     val lambda : AExp,
                     val body : CExp) extends CExp



class CPSProg {
}

object CPSProg {
}
