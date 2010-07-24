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
	val π = math.Pi

	// Clamp a value between a low and high bound
	@inline def clamp[@specialized(Int, Double) N : Ordering](value: N, low: N, high: N): N = {
		import Ordered._
		if (value < low) low else if (value > high) high else value
	}

	// Get the minimum and maximum of two values as a pair
	@inline def minmax[@specialized(Int, Double) N : Ordering](a: N, b: N): (N, N) = {
		import Ordered._
		if (a <= b) (a, b) else (b, a)
	}

	// Linearly interpolate a value
	@inline def interpolate(t: Float, a: Float, b: Float): Float = a * (1.0f - t) + b * t
	@inline def interpolate(t: Double, a: Double, b: Double): Double = a * (1.0 - t) + b * t

	// Round up to the next larger or equal power of 2
	def roundUpPow2(value: Int): Int = {
		if (value < 2) 2 else {
			var result = 0x40000000
			while (value <= result) { result >>= 1 }
			result << 1
		}
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

	// NOTE: In pbrt-v1, a naïve (biased) shuffling algorithm is used; in pbrt-v2, Fisher-Yates shuffling is used
	// See also http://www.codinghorror.com/blog/2007/12/the-danger-of-naivete.html
	// NOTE: Scala 2.8 does have a shuffle method in scala.util.Random, but it does not work on arrays (and doesn't do it in-place)
}
