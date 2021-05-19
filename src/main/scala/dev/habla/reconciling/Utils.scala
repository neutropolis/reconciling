package dev.habla.reconciling

import scala.quoted.*

given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

type Code[A] = Quotes ?=> Expr[A]

def debug[A](expr: Code[A]): Unit =
  println(s"Output: " + staging.run { q ?=>
    println(s"Expression: ${q.show(expr)}")
    expr 
  })

