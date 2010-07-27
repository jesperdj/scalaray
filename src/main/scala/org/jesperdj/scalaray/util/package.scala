/*
 * ScalaRay - Ray tracer based on pbrt (see http://pbrt.org) written in Scala 2.8
 * Copyright (C) 2009, 2010  Jesper de Jong
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

package object util {
	val π = math.Pi.toFloat

	// Clamp a value between a low and high bound
	@inline def clamp[@specialized(Int, Float) N : Ordering](value: N, low: N, high: N): N = {
		import Ordered._
		if (value < low) low else if (value > high) high else value
	}

	// Get the minimum and maximum of two values as a pair
	@inline def minmax[@specialized(Int, Float) N : Ordering](a: N, b: N): (N, N) = {
		import Ordered._
		if (a <= b) (a, b) else (b, a)
	}

	trait Multipliable[-T, +R] { def *(value: T): R }
	trait Addable[-T, +R] { def +(value: T): R }

	// Operations that a type must have to be used in interpolate()
	trait Interpolatable[T] extends Multipliable[Float, T] with Addable[T, T]

	// Linearly interpolate a value
	@inline def interpolate[@specialized(Float) T <% Interpolatable[T]](t: Float, a: T, b: T): T = a * (1.0f - t) + b * t

	// Implicit conversion to enable Float to be used in interpolate()
	implicit def floatToInterpolatable(f1: Float) = new Interpolatable[Float] {
		def *(t: Float): Float = f1 * t
		def +(f2: Float): Float = f1 + f2
	}

	// Used in for example package objects vecmath and spectrum
	trait ImplicitScale[T] extends Multipliable[T, T]

	// Randomly permutate an array - Fisher-Yates shuffle (see: http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle)
	// This method can also take a custom swap method, which is useful for example for Latin hypercube sampling
	def shuffle[@specialized(Float) T](array: Array[T], swap: (T, T) => (T, T) = { (a: T, b: T) => (b, a) }): Array[T] = {
		val random = new scala.util.Random

		for (n <- array.length - 1 to 0 by -1) {
			val k = random.nextInt(n + 1)
			val (a, b) = swap(array(k), array(n)); array(k) = a; array(n) = b
		}

		array
	}
}
