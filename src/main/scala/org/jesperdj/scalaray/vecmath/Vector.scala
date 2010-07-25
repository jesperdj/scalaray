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
package org.jesperdj.scalaray.vecmath

// Vector (pbrt 2.2)
final class Vector (val x: Float, val y: Float, val z: Float) {
	// Create a vector from a normal
	def this(n: Normal) = this(n.x, n.y, n.z)

	// Create a vector from a point
	def this(p: Point) = this(p.x, p.y, p.z)

	// Add two vectors
	def +(v: Vector) = new Vector(x + v.x, y + v.y, z + v.z)

	// Subtract two vectors
	def -(v: Vector) = new Vector(x - v.x, y - v.y, z - v.z)

	// Scale a vector
	def *(f: Float) = new Vector(x * f, y * f, z * f)
	def /(f: Float) = new Vector(x / f, y / f, z / f)

	// Unary minus
	def unary_- = new Vector(-x, -y, -z)

	// Dot product
	def *(v: Vector) = x * v.x + y * v.y + z * v.z

	// Dot product with a normal
	def *(n: Normal) = x * n.x + y * n.y + z * n.z

	// Cross product
	def **(v: Vector) = new Vector(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)

	// Cross product with a normal
	def **(n: Normal) = new Vector(y * n.z - z * n.y, z * n.x - x * n.z, x * n.y - y * n.x)

	// Length
	def length = math.sqrt(x * x + y * y + z * z).toFloat
	def lengthSquared = x * x + y * y + z * z

	// Normalize
	def normalize = this / length

	override def toString = "Vector(%g, %g, %g)" format (x, y, z)
}

object Vector {
	// Vector constants
	val XAxis = new Vector(1.0f, 0.0f, 0.0f)
	val YAxis = new Vector(0.0f, 1.0f, 0.0f)
	val ZAxis = new Vector(0.0f, 0.0f, 1.0f)
	val Zero = new Vector(0.0f, 0.0f, 0.0f)

	// Create a vector
	def apply(x: Float, y: Float, z: Float) = new Vector(x, y, z)

	// Create a vector from a normal
	def apply(n: Normal) = new Vector(n)

	// Create a vector from a point
	def apply(p: Point) = new Vector(p)

	// Coordinate system from a vector
	def coordinateSystem(v1: Vector): (Vector, Vector) = {
		val v2 = if (v1.x.abs > v1.y.abs) {
			val len = math.sqrt(v1.x * v1.x + v1.z * v1.z).toFloat
			new Vector(-v1.z / len, 0.0f, v1.x / len)
		}
		else {
			val len = math.sqrt(v1.y * v1.y + v1.z * v1.z).toFloat
			new Vector(0.0f, v1.z / len, -v1.y / len)
		}

		(v2, v1 ** v2)
	}
}
