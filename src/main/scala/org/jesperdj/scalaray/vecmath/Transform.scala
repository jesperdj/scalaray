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

// Transform (pbrt 2.7)
sealed class Transform private[vecmath] (private[vecmath] val mat: Matrix, private[vecmath] val inv: Matrix) {
	// Transform a point (pbrt 2.8.1)
	def *(p: Point) = mat * p

	// Transform a vector (pbrt 2.8.2)
	def *(v: Vector) = mat * v

	// Transform a normal (pbrt 2.8.3)
	def *(n: Normal) = inv * n

	// Transform a ray (pbrt 2.8.4)
	def *(r: Ray) = mat * r

	// Transform a ray differential (pbrt 2.8.4)
	def *(rd: RayDifferential) = mat * rd

	// Combine transforms (pbrt 2.8.6)
	def *(t: Transform) = new Transform(mat * t.mat, t.inv * inv)

	// Get the inverse of this transform (pbrt 2.7.2)
	def inverse = new Transform(inv, mat)

	// Check if this transform has a scale factor
	def hasScale: Boolean = {
		val det = mat(0, 0) * mat(1, 1) * mat(2, 2) + mat(0, 1) * mat(1, 2) * mat(2, 0) + mat(0, 2) * mat(1, 0) * mat(2, 1) -
				  mat(0, 0) * mat(1, 2) * mat(2, 1) - mat(0, 1) * mat(1, 0) * mat(2, 2) - mat(0, 2) * mat(1, 1) * mat(2, 0)
		det < 0.999f || det > 1.001f
	}

	override def toString = "Transform(mat=%s, inv=%s)" format (mat, inv)
}

object Transform {
	// Transform constants
	val Identity: Transform = new Transform(Matrix.Identity, Matrix.Identity) {
		override def *(p: Point) = p
		override def *(v: Vector) = v
		override def *(n: Normal) = n
		override def *(r: Ray) = r
    override def *(rd: RayDifferential) = rd
		override def *(t: Transform) = t
		override def inverse = this
    override def hasScale = false

		override def toString = "Transform.Identity"
	}

	// Create a translation (pbrt 2.7.3)
	def translate(x: Float, y: Float, z: Float) = new Transform(
		new Matrix(1.0f, 0.0f, 0.0f, x, 0.0f, 1.0f, 0.0f, y, 0.0f, 0.0f, 1.0f, z, 0.0f, 0.0f, 0.0f, 1.0f),
		new Matrix(1.0f, 0.0f, 0.0f, -x, 0.0f, 1.0f, 0.0f, -y, 0.0f, 0.0f, 1.0f, -z, 0.0f, 0.0f, 0.0f, 1.0f))

	// Create a translation using a vector (pbrt 2.7.3)
	def translate(v: Vector): Transform = translate(v.x, v.y, v.z)

	// Create a uniform scaling transform (pbrt 2.7.4)
	def scale(f: Float) = new Transform(
		new Matrix(f, 0.0f, 0.0f, 0.0f, 0.0f, f, 0.0f, 0.0f, 0.0f, 0.0f, f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f),
		new Matrix(1.0f / f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f / f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f / f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f))

	// Create a non-uniform scaling transform (pbrt 2.7.4)
	def scale(fx: Float, fy: Float, fz: Float) = new Transform(
		new Matrix(fx, 0.0f, 0.0f, 0.0f, 0.0f, fy, 0.0f, 0.0f, 0.0f, 0.0f, fz, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f),
		new Matrix(1.0f / fx, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f / fy, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f / fz, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f))

	// Create a rotation around the X axis (pbrt 2.7.5)
	def rotateX(angle: Float) = {
		val ca = math.cos(angle).toFloat
		val sa = math.sin(angle).toFloat
		new Transform(
			new Matrix(1.0f, 0.0f, 0.0f, 0.0f, 0.0f, ca, -sa, 0.0f, 0.0f, sa, ca, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f),
			new Matrix(1.0f, 0.0f, 0.0f, 0.0f, 0.0f, ca, sa, 0.0f, 0.0f, -sa, ca, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f))
	}

	// Create a rotation around the Y axis (pbrt 2.7.5)
	def rotateY(angle: Float) = {
		val ca = math.cos(angle).toFloat
		val sa = math.sin(angle).toFloat
		new Transform(
			new Matrix(ca, 0.0f, sa, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, -sa, 0.0f, ca, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f),
			new Matrix(ca, 0.0f, -sa, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, sa, 0.0f, ca, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f))
	}

	// Create a rotation around the Z axis (pbrt 2.7.5)
	def rotateZ(angle: Float) = {
		val ca = math.cos(angle).toFloat
		val sa = math.sin(angle).toFloat
		new Transform(
			new Matrix(ca, -sa, 0.0f, 0.0f, sa, ca, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f),
			new Matrix(ca, sa, 0.0f, 0.0f, -sa, ca, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f))
	}

	// Create a rotation around an arbitrary axis (pbrt 2.7.6)
	def rotate(angle: Float, axis: Vector) = {
		val a = axis.normalize
		val c = math.cos(angle).toFloat
		val s = math.sin(angle).toFloat

		val cc = 1.0f - c

		val t1 = a.x * a.y * cc
		val t2 = a.x * a.z * cc
		val t3 = a.y * a.z * cc

		val u1 = a.x * s
		val u2 = a.y * s
		val u3 = a.z * s

		val m = new Matrix(
			a.x * a.x * cc + c, t1 - u3, t2 + u2, 0.0f,
			t1 + u3, a.y * a.y * cc + c, t3 - u1, 0.0f,
			t2 - u2, t3 + u1, a.z * a.z * cc + c, 0.0f,
			0.0f, 0.0f, 0.0f, 1.0f)

		new Transform(m, m.transpose)
	}

	// Create a "look at" transform (pbrt 2.7.7)
	def lookAt(pos: Point, look: Point, up: Vector) = {
		val dir = (look - pos).normalize
		val left = (up.normalize ** dir).normalize
		val newUp = dir ** left

		val m = new Matrix(
			left.x, newUp.x, dir.x, pos.x,
			left.y, newUp.y, dir.y, pos.y,
			left.z, newUp.z, dir.z, pos.z,
			0.0f, 0.0f, 0.0f, 1.0f)

		new Transform(m.inverse, m)
	}

	// Orthographic transform (pbrt 6.2.1)
//	def orthographic(near: Float, far: Float) = scale(1.0f, 1.0f, 1.0f / (far - near)) * translate(0.0f, 0.0f, -near)

	// Perspective transform (pbrt 6.2.2)
//	def perspective(angleOfView: Float, near: Float, far: Float) = {
//		val s = 1.0f / math.tan(angleOfView * 0.5f).toFloat
//		val m = new Matrix(s, 0.0f, 0.0f, 0.0f, 0.0f, s, 0.0f, 0.0f, 0.0f, 0.0f, far / (far - near), -far * near / (far - near), 0.0f, 0.0f, 1.0f, 0.0f)
//		new Transform(m, m.inverse)
//	}
}
