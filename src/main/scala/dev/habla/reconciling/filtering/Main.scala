package dev.habla.reconciling
package filtering

import scala.reflect.ClassTag

import scala.quoted.*

def filter(b: Array[Float], x: Array[Float]): Array[Float] =
  val m = b.length
  def y(i: Int): Float =
    if i < m-1 then
      x(i)
    else
      var sum: Float = 0.0
      for k <- 0 to m-1 do
        sum += b(k) * x(i-k)
      sum
  Array.init(x.length, y)

extension (a: Array.type)
  // native in MetaOCaml
  def init[A: ClassTag](end: Int, f: Int => A): Array[A] =
    Array.range(0, end).map(f)

def impulses(i: Int, j: Int, n: Int): Array[Float] =
  val x = Array.fill(n)(0.0f)
  x.update(i, 1.0f)
  x.update(j, 1.0f)
  x

// specializing length

type SpineArr = Array[Code[Float]]
type DynArr = Code[Array[Float]]

def filterSpine(b: SpineArr): Code[Array[Float] => Array[Float]] = '{x =>
${val m = b.length
'{def y(i: Int): Float =
    if i < ${Expr(m)}-1 then
      x(i)
    else
    ${// XXX: can't use `sum = sum + ...` (as tutorial does) since that 
      // would lead to a StackOverflow error. This is essentially the same, but
      // we accumulate on an array and then we fold.
      val tosum: Array[Code[Float]] = Array.fill(m)('{0.0f})
      for k <- 0 to m-1 do
        tosum.update(k, '{${b(k)} * x(i-${Expr(k)})})
      tosum.foldLeft('{0.0f})((acc, c) => '{$acc + $c})} 
    // XXX: note that we can't use Array.init due to lack of CBS
    Array.range(0, x.length).map(y)}}
}

def filterSpine3: Code[(Float, Float, Float) => Array[Float] => Array[Float]] =
  '{ (b1, b2, b3) => ${filterSpine(Array[Code[Float]]('{b1}, '{b2}, '{b3}))} }

// specializing coefficients

def filterStaged(b: Array[Float]): Code[Array[Float] => Array[Float]] = '{x =>
${val m = b.length
'{def y(i: Int): Float =
    if i < ${Expr(m)}-1 then
      x(i)
    else
    ${val tosum: Array[Code[Float]] = Array.fill(m)('{0.0f})
      for k <- 0 to m-1 do
        tosum.update(k, '{${Expr(b(k))} * x(i-${Expr(k)})})
      tosum.foldLeft('{0.0f})((acc, c) => '{$acc + $c})} 
    Array.range(0, x.length).map(y)}}
}

// adaptative

// Test it!

@main def main: Unit =
  debug(filterStaged(Array[Float](0.5, 0.3, 0.2)))

