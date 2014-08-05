package net.might.matt.languages.reg ;

import net.might.matt.languages.sexp._ ;

/*

 <prog> ::= <stmt> ...

 <stmt> ::= (label <label>)
         |  (goto <label>) 
         |  (:= <var> <exp>)
         |  (if <exp> goto <label>)

 <exp> ::= (+ <exp> <exp>)
        |  (* <exp> <exp>)
        |  (= <exp> <exp>)
        |  <int>
        |  <var>
 
 */


abstract class Stmt {
}

object Stmt {
  def from(sexp : SExp) : Stmt = {
    sexp match {

      case SList(SSymbol("label"), SSymbol(target)) => 
       LabelStmt(target) 

      case SList(SSymbol("goto"), SSymbol(target)) =>
       GotoStmt(target)  

      case SList(SSymbol("if"),cond,SSymbol("goto"),SSymbol(target)) =>
       IfStmt(Exp.from(cond), target)

      case SList(SSymbol(":="),SSymbol(dst),src) =>
       AssignStmt(dst,Exp.from(src)) 

      case _ => throw new RuntimeException("unfinished: " + sexp) ;

    } 
  }
}



abstract class Exp {
}

object Exp {

  def from (sexp : SExp) : Exp = {
    sexp match {
      case SInt(value) => IntExp(value) 

      case SSymbol(id) => RefExp(id)

      case SList(SSymbol("+"),a,b) =>
       PlusExp(from(a), from(b))

      case SList(SSymbol("*"),a,b) =>
       TimesExp(from(a),from(b))

      case SList(SSymbol("="),a,b) =>
       EqExp(from(a), from(b))


    }
  }
}


/* Statement forms */

case class LabelStmt(val target : String) extends Stmt

case class IfStmt(val cond : Exp, val target : String) extends Stmt

case class AssignStmt(val lhs : String, val rhs : Exp) extends Stmt

case class GotoStmt(val target : String) extends Stmt


/* Expression forms */

case class IntExp(val value : Int) extends Exp

case class RefExp(val id : String) extends Exp

case class PlusExp(val a : Exp, val b : Exp) extends Exp

case class TimesExp(val a : Exp, val b : Exp) extends Exp

case class EqExp(val a : Exp, val b : Exp) extends Exp


object RegProg {
  def from (reg : SExp) : RegProg = new RegProg(reg.toList map Stmt.from) 
}

case class RegProg(val stmts : List[Stmt]) {
}
