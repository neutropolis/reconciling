package dev.habla.reconciling
package compiler

import scala.quoted.*

enum Exp:
  case Num(i: Int)
  case Bool(b: Boolean)
  case Gt(l: Exp, r: Exp)
  case If(cnd: Exp, thn: Exp, els: Exp)

import Exp.*

val ex: Exp =
  If(Gt(Num(9), Num(3)), thn = Num(1), els = Num(0))

val wr_ex: Exp =
  If(Num(3), thn = Num(1), els = Num(0))

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
      case _ => throw Error("If statement requires a boolean condition")
  }
  case _ => throw Error("Unexpected error")

// Expression compiler (~ macro)

@main def main: Unit =
  debug(compile(ex))

