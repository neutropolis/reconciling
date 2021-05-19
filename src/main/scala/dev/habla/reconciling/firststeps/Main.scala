package dev.habla.reconciling
package firststeps

import scala.quoted.*

// Now or later

val v: Int = 1 + 2

val c: Code[Int] = '{1 + 2}

def times10(x: Code[Int]): Code[Int] = '{$x * 10}

// Power

def square(x: Int): Int = x * x

def power(n: Int, x: Int): Int = n match
  case 0 => 1
  case n if n % 2 == 0 => square(power(n/2, x))
  case n => x * power(n-1, x)

def power7(x: Int): Int = power(7, x)

def ssquare(x: Code[Int]): Code[Int] = '{$x * $x}

def spower(n: Int, x: Code[Int]): Code[Int] = n match
  case 0 => '{1}
  case 1 => x
  // XXX: Scala 3 doesn't support CSP (as in metaocaml), so can't refer to square
  // case n if n % 2 == 0 => '{square(${spower(n/2, x)})}
  case n if n % 2 == 0 => ssquare(spower(n/2, x))
  case n => '{$x * ${spower(n-1, x)}}

def spowern(n: Int): Code[Int => Int] =
  '{ x => ${spower(n, 'x)} }

def spowern7: Int => Int = x => staging.run(spowern(7))(x)

// Test it!

@main def main: Unit =
  debug(spowern(7))
  println(spowern7(2))

