package dev.habla.reconciling
package compiler

import scala.quoted.*
import scala.compiletime.error

enum Exp:
  case Num(i: Int)
  case Bool(b: Boolean)
  case Gt(l: Exp, r: Exp)
  case If(cnd: Exp, thn: Exp, els: Exp)

import Exp.*

val ex: Exp =
  If(Gt(Num(9), Num(3)), thn = Num(1), els = Num(0))


val ex2: Exp =
  If(Gt(If(Gt(Num(9), Num(3)), Num(9), Num(10)), Num(3)), thn = Num(1), els = Num(0))

val wr_ex: Exp =
  If(Num(3), thn = Num(1), els = Num(0))


val wr_ex2: Exp =
  Gt(Gt(Num(1), Num(2)), Num(3))

// Expression interpreter

type Value = Either[Int, Boolean]

def interpret(e: Exp): Value = e match
  case Num(i) => Left(i)
  case Bool(b) => Right(b)
  case Gt(Num(l), Num(r)) => Right(l > r)
  case If(cnd, thn, els) => interpret(cnd) match
    case Right(b) => if b then interpret(thn) else interpret(els)
    case _ => throw Error(s"Expected boolean condition, but found '$cnd'")
  case _ => throw Error("Unexpected error")

// Expression compiler (~ runtime)

def compile(e: Exp): Code[Value] = e match
  case Num(i) => '{Left(${Expr(i)})}
  case Bool(b) => '{Right(${Expr(b)})}
  case Gt(Num(l), Num(r)) => '{Right(${Expr(l > r)})}
  case If(cnd, thn, els) => '{
    ${compile(cnd)} match
      case Right(b) => if b then ${compile(thn)} else ${compile(els)}
      case null => throw Error("If statement requires a boolean condition")
  }
  case _ => throw Error("Unexpected error")

type ValueUnion = Int | Boolean

def interpretU(e: Exp): ValueUnion = e match
  case Num(i) => i
  case Bool(b) => b
  case Gt(Num(l), Num(r)) => l > r
  case Gt(l, r) => (interpretU(l), interpretU(r)) match
    case (li, ri): (Int, Int) => li > ri
    case _ => throw Error("GT error")
  case If(cnd, thn, els) => interpretU(cnd) match
    case b : Boolean => if b then interpretU(thn) else interpretU(els)
    case _ => throw Error(s"Expected boolean condition, but found '$cnd'")

// Expression compiler (~ runtime)

def compileU(e: Exp): Code[ValueUnion] = e match
  case Num(i) => Expr(i)
  case Bool(b) => Expr(b)
  case Gt(Num(l), Num(r)) => '{${Expr(l)} > ${Expr(r)}}
  case Gt(l, r) => (compileU(l), compileU(r)) match
    case ('{$lc: Int}, '{$rc: Int}) => '{${lc} > ${rc}}
    case (a, b) =>
      quotes.reflect.report.error(s"GT must be formed with numeric values:\n" +
        s"left: ${quotes.show(a)}\n" +
        s"right: ${quotes.show(b)}")
      Expr(true)
  case If(cnd, thn, els) => compileU(cnd) match
    case '{$bc: Boolean} => '{if ${bc} then ${compileU(thn)} else ${compileU(els)}}
    case x =>
      println(summon[Quotes].show(compileU(cnd)))
      quotes.reflect.report.error("If statement requires a boolean condition\n" +
      s"found: ${quotes.show(x)}")
      Expr(0)


object TermF:
  def unapply(using Quotes)(expr: Expr[_]): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    Some(expr.asTerm)

def compileUOptimized(e: Exp): Code[ValueUnion] =
  import quotes.reflect.*
  e match
    case Num(i) => Expr(i)
    case Bool(b) => Expr(b)
    case Gt(Num(l), Num(r)) => Expr(l > r)
    case Gt(l, r) =>
      val a: Term = compileUOptimized(l).asTerm
      (compileUOptimized(l), compileUOptimized(r)) match
          // if both elements are constant int's
        case (TermF(Literal(IntConstant(lc))), TermF(Literal(IntConstant(rc)))) => Expr(lc > rc)
          // if both elements are integer
        case ('{$lc: Int}, '{$rc: Int}) => '{${lc} > ${rc}}
        case (a, b) =>
          quotes.reflect.report.error(s"GT must be formed with numeric values:\n" +
            s"left: ${quotes.show(a)}\n" +
            s"right: ${quotes.show(b)}")
            Expr(true)

    case Exp.If(cnd, thn, els) => compileUOptimized(cnd) match
        // if the condition is a constant
      case '{true} => compileUOptimized(thn)
      case '{false} => compileUOptimized(els)
        // if not a constant
      case '{$bc: Boolean} => '{if ${bc} then ${compileUOptimized(thn)} else ${compileUOptimized(els)}}
      case x =>
        quotes.reflect.report.error(s"If statement requires a boolean condition\nfound: ${quotes.show(x)}")
        Expr(0)


// Expression compiler (~ macro)

@main def main: Unit =
  println(interpretU(ex2))
  //debug(compile(ex))
  //showCode(compile(ex))
  debug(compileU(ex2))
  debug(compileUOptimized(ex2))
  //debug(compileU(wr_ex))
  //debug(compileU(wr_ex2))
  //debug(compileU(Gt(Num(3), Num(5))))

