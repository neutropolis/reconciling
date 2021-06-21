package dev.habla.reconciling
package algebra

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

given Ring[Code[Float]] with
  val zero: Code[Float] = '{0.0f}
  val one: Code[Float] = '{1.0f}
  def add(x: Code[Float], y: Code[Float]): Code[Float] = '{$x + $y}
  def sub(x: Code[Float], y: Code[Float]): Code[Float] = '{$x - $y}
  def mul(x: Code[Float], y: Code[Float]): Code[Float] = '{$x * $y}

val xsq1_code: Code[Float] = xsq1[Code[Float]]('{2.0f})

case class Complex[A](re: A, im: A)

given RingComplex[A](using R: Ring[A]): Ring[Complex[A]] with
  val zero: Complex[A] = Complex(R.zero, R.zero)
  val one: Complex[A] = Complex(R.one, R.zero)
  def add(x: Complex[A], y: Complex[A]): Complex[A] =
    Complex(R.add(x.re, y.re), R.add(x.im, y.im))
  def sub(x: Complex[A], y: Complex[A]): Complex[A] =
    Complex(R.sub(x.re, y.re), R.sub(x.im, y.im))
  def mul(x: Complex[A], y: Complex[A]): Complex[A] =
    Complex(
      R.sub(R.mul(x.re, y.re), R.mul(x.im, y.im)),
      R.add(R.mul(x.re, y.im), R.mul(x.im, y.re)))

type float_complex = Complex[Float]
val FloatComplex = summon[Ring[float_complex]]

val xsq1_float_complex: Complex[Float] = xsq1(Complex(2.0f, 1.0f))

type float_code_complex = Complex[Code[Float]]
val FloatCodeComplex = summon[Ring[float_code_complex]]

def of_complex_code: Code[Complex[Float]] => Complex[Code[Float]] =
  x => Complex('{$x.re}, '{$x.im})

def of_code_complex: Complex[Code[Float]] => Code[Complex[Float]] =
  case Complex(re, im) => '{Complex($re, $im)}

val xsq1_float_code_complex: Code[Complex[Float] => Complex[Float]] =
  '{ (x: Complex[Float]) => ${ of_code_complex(xsq1(of_complex_code('x))) } }


// Abstracting vectors

case class Vec[I, A](len: I, at: I => A)

case class OVec[I, A, O](len: I, mutate: I => A => O)

def vec_of_array[A](arr: Array[A]): (Vec[Int, A], OVec[Int, A, Unit]) =
  (Vec(arr.length, arr.apply), OVec(arr.length, arr.update.curried))

def vec_map[A, B, I](f: A => B)(vec: Vec[I, A]): Vec[I, B] =
  Vec(vec.len, f compose vec.at)

def zip_with[A, B, C, I](f: A => B => C)(v: Vec[I, A], w: Vec[I, B]): Vec[I, C] =
  Vec(v.len, i => f(v.at(i))(w.at(i)))

def vec_assign[I, A, O](ov: OVec[I, A, O], v: Vec[I, A]): Vec[I, O] =
  Vec(ov.len, i => ov.mutate(i)(v.at(i)))

trait VECT[Idx, Unt]:
  def iter(v: Vec[Idx, Unt]): Unt

given VECT[Int, Unit] with
  def iter(v: Vec[Int, Unit]): Unit =
    for (i <- 0 to v.len) v.at(i)

extension [I, A, O] (vout: OVec[I, A, O]) def :=(vin: Vec[I, A])(using A: VECT[I, O]): O =
  A.iter(vec_assign(vout, vin))

extension [I, A] (v1: Vec[I, A]) def *(v2: Vec[I, A])(using R: Ring[A]): Vec[I, A] =
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
  val vout = OVec(n, aout.update.curried)
  val v1 = Vec(n, a1.apply)
  val v2 = Vec(n, a2.apply)
  vmult(vout, v1, v2)

