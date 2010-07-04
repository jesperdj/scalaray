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
package org.jesperdj

package object scalaray {
	val π = math.Pi

	// Clamp a value between a low and high bound
	def clamp[N : Ordering](value: N, low: N, high: N): N = {
		import Ordered._
		if (value < low) low else if (value > high) high else value
	}

	// Linearly interpolate a value
	def interpolate(t: Double, a: Double, b: Double): Double = a * (1.0 - t) + b * t

	// Get the minimum and maximum of two values as a pair
	def minmax[N : Ordering](a: N, b: N): (N, N) = {
		import Ordered._
		if (a <= b) (a, b) else (b, a)
	}

	// Randomly permutate an array - Fisher-Yates shuffle (see: http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle)
	// NOTE: Scala 2.8 does have a shuffle method in scala.util.Random, but it does not work on arrays (and doesn't do it in-place)
	def shuffle[T](array: Array[T]): Array[T] = {
		val random = new scala.util.Random

		for (n <- array.length - 1 to 0 by -1) {
			val k = random.nextInt(n + 1)
			val t = array(k); array(k) = array(n); array(n) = t
		}

		array
	}
}
