/*
 * ScalaRay - Ray tracer based on pbrt (see http://pbrt.org) written in Scala
 * Copyright (C) 2009, 2010, 2011  Jesper de Jong
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.jesperdj.scalaray

import scala.collection.immutable.IndexedSeq

package object common {
  val π = math.Pi

  // Trait for types that can be multiplied with a T, resulting in an R
  trait Multipliable[-T, +R] {
    def *(value: T): R
  }

  // Trait for types to which a T can be added, resulting in an R
  trait Addable[-T, +R] {
    def +(value: T): R
  }

  trait MultipliableSame[T] extends Multipliable[T, T]

  trait AddableSame[T] extends Addable[T, T]

  trait Interpolatable[T] extends Multipliable[Double, T] with AddableSame[T]

  implicit def doubleToInterpolatable(n: Double) = new Interpolatable[Double] {
    @inline def *(value: Double): Double = n * value
    @inline def +(value: Double): Double = n + value
  }

  // Linearly interpolate between two values
  @inline def interpolate[@specialized(Double) T <% Interpolatable[T]](t: Double, a: T, b: T): T = a * (1.0 - t) + b * t

  @inline def clamp[@specialized(Int, Double) T : Ordering](value: T, low: T, high: T): T = {
    import Ordered._
    if (value < low) low else if (value > high) high else value
  }

  @inline def minmax[@specialized(Int, Double) T : Ordering](a: T, b: T): (T, T) = {
    import Ordered._
    if (a <= b) (a, b) else (b, a)
  }

  // Create an immutable IndexedSeq that wraps an Array. Note that Scala already contains a method wrapDoubleArray(), but this returns a mutable WrappedArray.
  // Also, this version is @specialized on Double to avoid unnecessary boxing and unboxing.
  def arrayToIndexedSeq[@specialized(Double) T](array: Array[T]): IndexedSeq[T] = new IndexedSeq[T] {
    def apply(idx: Int): T = array(idx)
    def length: Int = array.length
  }

  // Randomly permutate an array - Fisher-Yates shuffle (see: http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle)
  // This method can also take a custom swap method, which is useful for example for Latin hypercube sampling
  def shuffle[@specialized(Double) T](array: Array[T], swap: (T, T) => (T, T) = { (a: T, b: T) => (b, a) }): Array[T] = {
    val random = new scala.util.Random

    for (n <- array.length - 1 to 0 by -1) {
      val k = random.nextInt(n + 1)
      val (a, b) = swap(array(k), array(n)); array(k) = a; array(n) = b
    }

    array
  }
}
