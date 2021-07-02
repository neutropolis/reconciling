package dev.habla.reconciling
package algebra

import scala.quoted.*

// Our goal is to write code in the clear, obviously correct way, in the
// notation close to the one used by domain experts and then interpret that
// notation as a code generator, adding optimizations one-by-one.
//
// Tagless final => makes optimizations modular and composable
//               => defined as a mapping of DSL exprs to host values (denotations)
//               => compositional

trait Ring[T]:
  val zero: T
  val one: T
  def add(x: T, y: T): T
  def sub(x: T, y: T): T
  def mul(x: T, y: T): T

def xsq1[T](x: T)(using R: Ring[T]): T =
  import R.*
  add(mul(x, x), one)

given Ring[Float] with
  val zero: Float = 0.0
  val one: Float = 1.0
  def add(x: Float, y: Float): Float = x + y
  def sub(x: Float, y: Float): Float = x - y
  def mul(x: Float, y: Float): Float = x * y

val xsq1_float: Float = xsq1(2.0f)

given RingFloatCode: Ring[Code[Float]] with
  val zero: Code[Float] = '{0.0f}
  val one: Code[Float] = '{1.0f}
  def add(x: Code[Float], y: Code[Float]): Code[Float] = '{$x + $y}
  def sub(x: Code[Float], y: Code[Float]): Code[Float] = '{$x - $y}
  def mul(x: Code[Float], y: Code[Float]): Code[Float] = '{$x * $y}

val xsq1_code: Code[Float] = xsq1[Code[Float]]('{2.0f})

type Complex[A] = (A, A)

given RingComplex[A](using R: Ring[A]): Ring[Complex[A]] with
  val zero: Complex[A] = (R.zero, R.zero)
  val one: Complex[A] = (R.one, R.zero)
  def add(x: Complex[A], y: Complex[A]): Complex[A] =
    (R.add(x._1, y._1), R.add(x._2, y._2))
  def sub(x: Complex[A], y: Complex[A]): Complex[A] =
    (R.sub(x._1, y._1), R.sub(x._2, y._2))
  def mul(x: Complex[A], y: Complex[A]): Complex[A] =
    (R.sub(R.mul(x._1, y._1), R.mul(x._2, y._2)),
     R.add(R.mul(x._1, y._2), R.mul(x._2, y._1)))

type float_complex = Complex[Float]
val FloatComplex = summon[Ring[float_complex]]

val xsq1_float_complex: Complex[Float] = xsq1((2.0f, 1.0f))

type float_code_complex = Complex[Code[Float]]
val FloatCodeComplex = summon[Ring[float_code_complex]]

def of_complex_code: Code[Complex[Float]] => Complex[Code[Float]] =
  x => ('{$x._1}, '{$x._2})

def of_code_complex: Complex[Code[Float]] => Code[Complex[Float]] =
  case (re, im) => '{($re, $im)}

val xsq1_float_code_complex: Code[Complex[Float] => Complex[Float]] =
  '{ (x: Complex[Float]) => ${ of_code_complex(xsq1(of_complex_code('x))) } }


// Abstracting vectors

type Vec[I, A] = (I, I => A)

extension [I, A] (v: Vec[I, A])
  def len: I = v._1
  def at: I => A = v._2

type OVec[I, A, O] = (I, I => A => O)

extension [I, A, O] (v: OVec[I, A, O])
  inline def len: I = v._1
  inline def mutate: I => A => O = v._2

def vec_of_array[A](arr: Array[A]): (Vec[Int, A], OVec[Int, A, Unit]) =
  ((arr.length, arr.apply), (arr.length, arr.update.curried))

def vec_map[A, B, I](f: A => B)(vec: Vec[I, A]): Vec[I, B] =
  (vec.len, f compose vec.at)

def zip_with[A, B, C, I](f: A => B => C)(v: Vec[I, A], w: Vec[I, B]): Vec[I, C] =
  (v.len, i => f(v.at(i))(w.at(i)))

def vec_assign[I, A, O](ov: OVec[I, A, O], v: Vec[I, A]): Vec[I, O] =
  (ov.len, i => ov.mutate(i)(v.at(i)))

trait VECT[Idx, Unt]:
  def iter(v: Vec[Idx, Unt]): Unt

given VecSta: VECT[Int, Unit] with
  def iter(v: Vec[Int, Unit]): Unit =
    for (i <- 0 to (v.len-1)) v.at(i)

extension [I, A, O] (vout: OVec[I, A, O])
  def :=(vin: Vec[I, A])(using A: VECT[I, O]): O = A.iter(vec_assign(vout, vin))

extension [I, A] (v1: Vec[I, A])
  def *(v2: Vec[I, A])(using R: Ring[A]): Vec[I, A] =
    zip_with(R.mul.curried)(v1, v2)

def vmult[I, A, O](
    vout: OVec[I, A, O],
    v1: Vec[I, A],
    v2: Vec[I, A])(using
    A: VECT[I, O],
    R: Ring[A]): O =
  vout := v1 * v2

def vmult(
    aout: Array[Complex[Float]],
    a1: Array[Complex[Float]],
    a2: Array[Complex[Float]]): Unit =
  val n = aout.length
  val vout = (n, aout.update.curried)
  val v1 = (n, a1.apply)
  val v2 = (n, a2.apply)
  vmult(vout, v1, v2)


// Compiling vector DSL

given VecDyn: VECT[Code[Int], Code[Unit]] with
  def iter(v: Vec[Code[Int], Code[Unit]]): Code[Unit] =
    '{ for (i <- 0 to ${v._1}-1) ${v._2('i)} }

// TODO: Not working!

val vmult_ca: Code[
    Array[Complex[Float]] =>
    Array[Complex[Float]] =>
    Array[Complex[Float]] =>
    Unit] = '{ aout => a1 => a2 =>
  val n = aout.length
  ${
    val vout: OVec[Code[Int], Complex[Code[Float]], Code[Unit]] = 
      ('n, i => v => '{aout.update($i, ${of_code_complex(v)})})
    val v1: Vec[Code[Int], Complex[Code[Float]]] =
      ('n, i => of_complex_code('{a1($i)}))
    val v2: Vec[Code[Int], Complex[Code[Float]]] =
      ('n, i => of_complex_code('{a2($i)}))
    vmult(vout, v1, v2)(using VecDyn, FloatCodeComplex)
  }
}

val vmult_ac: Code[
    Complex[Array[Float]] =>
    Complex[Array[Float]] =>
    Complex[Array[Float]] =>
    Unit] = '{ aout => a1 => a2 =>
  val n = aout._1.length
  ${
    val vout: OVec[Code[Int], Complex[Code[Float]], Code[Unit]] = 
      ('n, i => v => '{ aout._1.update($i, ${v._1}); aout._2.update($i, ${v._2}) })
    val v1: Vec[Code[Int], Complex[Code[Float]]] =
      ('n, i => ('{a1._1($i)}, '{a1._2($i)}))
    val v2: Vec[Code[Int], Complex[Code[Float]]] =
      ('n, i => ('{a2._1($i)}, '{a2._2($i)}))
    vmult(vout, v1, v2)(using VecDyn, FloatCodeComplex)
  }
}

// When designing the domain abstractions, we should think only of what makes
// the most sense to domain expert. We should not worry about the cost of
// abstractions in terms of creating closures and functors and indirections

