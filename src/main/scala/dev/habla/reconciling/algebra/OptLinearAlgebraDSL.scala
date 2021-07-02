package dev.habla.reconciling
package algebra

import scala.quoted.*

// We thus extend the systematic approach to program specialization introduced
// in the previous section, tackling not just spe- cializations but modular
// optimizations.

// ... to maintain static assurances, does not let us inspect, let alone
// rearrange, the generated code. Nevertheless, optimizations are possible;
// moreover, they are modular and assuredly sane.

// Shonan challenge 1: matrix-vector multiplication

trait VectR[I, O, T] extends VECT[I, O]:
  def reduce(r: T => T => T)(z: T)(v: Vec[I, T]): T 

def dot[I, A, O](
    v1: Vec[I, A])(
    v2: Vec[I, A])(using
    R: Ring[A],
    A: VectR[I, O, A]): A =
  A.reduce(R.add.curried)(R.zero)(v1 * v2)

type Matrix[I, A] = Vec[I, Vec[I, A]]

extension [I, A, O] (a: Matrix[I, A])
  def **(v: Vec[I, A])(using R: Ring[A], A: VectR[I, O, A]): Vec[I, A] =
    vec_map(dot(v))(a)

def mvmult[I, A, O](
    vout: OVec[I, A, O],
    a: Matrix[I, A],
    v: Vec[I, A])(using
    R: Ring[A],
    A: VectR[I, O, A],
    O: VECT[I, O]): O =
  vout := a ** v

given VecRSta[T]: VectR[Int, Unit, T] with

  def iter(v: Vec[Int, Unit]): Unit = VecSta.iter(v)

  def reduce(plus: T => T => T)(zero: T)(v: Vec[Int, T]): T =
    var sum = zero
    for (i <- 0 to (v._1 - 1))
      sum = plus(sum)(v._2(i))
    sum

def mvmult_p(vout: Array[Float], a: Array[Array[Float]], v: Array[Float]): Unit =
  val n = vout.length; val m = v.length
  val xout: OVec[Int, Float, Unit] = (n, vout.update.curried)
  val xa: Matrix[Int, Float] = (n, i => (m, j => a(i)(j)))
  val xv: Vec[Int, Float] = (m, v.apply)
  mvmult(xout, xa, xv)

val a: Array[Array[Float]] = Array(
  Array[Float](0.5, 0.0, 0.0, 0.5, 0.0),
  Array[Float](0.0, 0.0, 1.0, 0.0, 0.0),
  Array[Float](0.0, 1.0, 0.0, 0.0, 0.0),
  Array[Float](0.0, 0.0, 0.2, 0.3, 0.5),
  Array[Float](0.0, 0.0, 0.3, 0.0, 0.7))

val v1: Array[Float] = Array[Float](1.0, 2.0, 3.0, 4.0, 5.0)

val v1out: Array[Float] =
  val vout: Array[Float] = Array.ofDim[Float](5)
  mvmult_p(vout, a, v1)
  vout

given VecRDyn[T: Type]: VectR[Code[Int], Code[Unit], Code[T]] with

  def iter(v: Vec[Code[Int], Code[Unit]]): Code[Unit] = VecDyn.iter(v)

  def reduce(
      plus: Code[T] => Code[T] => Code[T])(
      zero: Code[T])(
      v: Vec[Code[Int], Code[T]]): Code[T] =
    '{ var sum = $zero
       for (i <- 0 to (${v._1} - 1))
         sum = ${plus('{sum})(v._2('{i}))}
       sum }

val mvmult_c: Code[Array[Float] => Array[Array[Float]] => Array[Float] => Unit] =
  '{ vout => a => v =>
    val n = vout.length; val m = v.length
    ${
      val xout: OVec[Code[Int], Code[Float], Code[Unit]] =
        ('{n}, i => a => '{vout.update($i, $a)})
      val xa: Matrix[Code[Int], Code[Float]] =
        ('{n}, i => ('{m}, j => '{a($i)($j)}))
      val xv: Vec[Code[Int], Code[Float]] = ('{m}, i => '{v($i)}) 
      mvmult(xout, xa, xv)(using RingFloatCode, VecRDyn, VecRDyn)
    }
  }

given VecRStaDim[T: Type]: VectR[Int, Code[Unit], Code[T]] with

  def reduce(
      plus: Code[T] => Code[T] => Code[T])(
      zero: Code[T])(
      v: Vec[Int, Code[T]]): Code[T] =
    VecRSta.reduce(plus)(zero)(v)

  // TODO: this is wrong!
  def iter(v: Vec[Int, Code[Unit]]): Code[Unit] =
    '{ for (i <- 0 to (${Expr(v.len)}-1)) ${v.at('{i}.valueOrError)} }

type PV[A] = Either[A, Code[A]]

trait Lift[T]:
  def lift(t: T): Code[T]

given Lift[Int] with
  def lift(i: Int): Code[Int] = Expr(i)

given Lift[Float] with
  def lift(f: Float): Code[Float] = Expr(f)

def dyn[A](pv: PV[A])(using L: Lift[A]): Code[A] = pv.fold(L.lift, identity)

// Useless in Scala but...
def dyni(pv: PV[Int]): Code[Int] = dyn(pv)
def dynf(pv: PV[Float]): Code[Float] = dyn(pv)

given RingPV[A](using R: Ring[A], R2: Ring[Code[A]], L: Lift[A]): Ring[PV[A]] with
  val zero: PV[A] = Left(R.zero)
  val one: PV[A] = Left(R.one)
  def add(x: PV[A], y: PV[A]): PV[A] = (x, y) match
    case (Left(a1), Left(a2)) => Left(R.add(a1, a2))
    case (x, y) => Right(R2.add(dyn(x), dyn(y)))
  def sub(x: PV[A], y: PV[A]): PV[A] = (x, y) match
    case (Left(a1), Left(a2)) => Left(R.sub(a1, a2))
    case (x, y) => Right(R2.sub(dyn(x), dyn(y)))
  def mul(x: PV[A], y: PV[A]): PV[A] = (x, y) match
    case (Left(a1), Left(a2)) => Left(R.mul(a1, a2))
    case (x, y) => Right(R2.mul(dyn(x), dyn(y)))

