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

// Quaternion (pbrt 2.9.1)
final class Quaternion (val v: Vector, val w: Double) {
	// Create a quaternion from components
	def this(x: Double, y: Double, z: Double, w: Double) = this(new Vector(x, y, z), w)

	// Add two quaternions
	def +(q: Quaternion) = new Quaternion(v + q.v, w + q.w)

	// Subtract two quaternions
	def -(q: Quaternion) = new Quaternion(v - q.v, w - q.w)

	// Scale a quaternion
	def *(f: Double) = new Quaternion(v * f, w * f)
	def /(f: Double) = new Quaternion(v / f, w / f)

	// Dot product
	def *(q: Quaternion) = v * q.v + w * q.w

	// Length
	def length = math.sqrt(this * this)
	def lengthSquared = this * this

	// Normalize
	def normalize = this / length

	// Rotation angle of this quaternion
	def angle = 2.0 * math.acos(w)

	// Rotation axis of this quaternion
	def axis: Vector = v.normalize

	// Convert to a transform
	def toTransform = {
		val xx = v.x * v.x; val yy = v.y * v.y; val zz = v.z * v.z
		val xy = v.x * v.y; val xz = v.x * v.z; val yz = v.y * v.z
		val wx = v.x * w; val wy = v.y * w; val wz = v.z * w

		val m = new Matrix(
			1.0 - 2.0 * (yy + zz), 2.0 * (xy + wz), 2.0 * (xz - wy), 0.0,
			2.0 * (xy - wz), 1.0 - 2.0 * (xx + zz), 2.0 * (yz + wx), 0.0,
			2.0 * (xz + wy), 2.0 * (yz - wx), 1.0 - 2.0 * (xx + yy), 0.0,
			0.0, 0.0, 0.0, 1.0)

		// Transpose because we are using a left-handed coordinate system
		new Transform(m.transpose, m)
	}

	override def toString = "Quaternion(%g, %g, %g, %g)" format (v.x, v.y, v.z, w)
}

object Quaternion {
	// Quaternion constants
	val Identity = new Quaternion(Vector.Zero, 1.0)
	val Zero = new Quaternion(Vector.Zero, 0.0)

	// Create a quaternion
	def apply(v: Vector, w: Double) = new Quaternion(v, w)

	// Create a quaternion from components
	def apply(x: Double, y: Double, z: Double, w: Double) = new Quaternion(new Vector(x, y, z), w)

	// Create a quaternion from a transform
	def apply(t: Transform): Quaternion = {
		val trace = t.mat(0, 0) + t.mat(1, 1) + t.mat(2, 2)
		if (trace > 0.0) {
			val s = 2.0 * math.sqrt(trace + 1.0)

			if (s == 0.0) Zero
			else new Quaternion((t.mat(2, 1) - t.mat(1, 2)) / s, (t.mat(0, 2) - t.mat(2, 0)) / s, (t.mat(1, 0) - t.mat(0, 1)) / s, s / 4.0)
		}
		else {
			// Find index of largest trace component
			var i = 0
			if (t.mat(1, 1) > t.mat(0, 0)) i = 1
			if (t.mat(2, 2) > t.mat(i, i)) i = 2

			val nxt: Array[Int] = Array(1, 2, 0)
			val j = nxt(i)
			val k = nxt(j)

			val s = 2.0 * math.sqrt((t.mat(i, i) - (t.mat(j, j) + t.mat(k, k))) + 1.0)

			if (s == 0.0) Zero
			else {
				val q: Array[Double] = new Array(3)
				q(i) = s / 4.0
				q(j) = (t.mat(j, i) + t.mat(i, j)) / s
				q(k) = (t.mat(k, i) + t.mat(i, k)) / s

				new Quaternion(q(0), q(1), q(2), (t.mat(k, j) - t.mat(j, k)) / s)
			}
		}
	}
}
