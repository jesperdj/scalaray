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

// NOTE: The main reason to have a separate class Normal (instead of using Vector for normals) is because normals
// have to be transformed differently: by multiplying them with the transpose of the inverse (see pbrt 2.8.3).

// Normal (pbrt 2.4)
final class Normal (val x: Float, val y: Float, val z: Float) {
	// Create a normal from a vector
	def this(v: Vector) = this(v.x, v.y, v.z)

	// Create a normal from a point
	def this(p: Point) = this(p.x, p.y, p.z)

	// Add two normals
	def +(n: Normal) = new Normal(x + n.x, y + n.y, z + n.z)

	// Subtract two normals
	def -(n: Normal) = new Normal(x - n.x, y - n.y, z - n.z)

	// Scale a normal
	def *(f: Float) = new Normal(x * f, y * f, z * f)
	def /(f: Float) = new Normal(x / f, y / f, z / f)

	// Unary minus
	def unary_- = new Normal(-x, -y, -z)

	// Dot product
	def *(n: Normal) = x * n.x + y * n.y + z * n.z

	// Dot product with a vector
	def *(v: Vector) = x * v.x + y * v.y + z * v.z

	// Cross product with a vector
	def **(v: Vector) = new Vector(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)

	// Length
	def length = math.sqrt(this * this).toFloat
	def lengthSquared = this * this

	// Normalize
	def normalize = this / length

	override def toString = "Normal(%g, %g, %g)" format (x, y, z)
}

object Normal {
	// Normal constants
	val XAxis = new Normal(1.0f, 0.0f, 0.0f)
	val YAxis = new Normal(0.0f, 1.0f, 0.0f)
	val ZAxis = new Normal(0.0f, 0.0f, 1.0f)
	val Zero = new Normal(0.0f, 0.0f, 0.0f)

	// Create a normal
	def apply(x: Float, y: Float, z: Float) = new Normal(x, y, z)

	// Create a normal from a vector
	def apply(v: Vector) = new Normal(v)

	// Create a normal from a point
	def apply(p: Point) = new Normal(p)
}
