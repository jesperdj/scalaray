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

import scala.collection.immutable._

// Matrix, used for transformations
private sealed class Matrix (elems: IndexedSeq[Double]) {
	require(elems.length == 16, "A Matrix requires exactly 16 elements")

	// Create a matrix (using varargs)
	def this(elems: Double*) = this(IndexedSeq(elems: _*))

	// Access a matrix element
	def apply(row: Int, col: Int) = elems(row * 4 + col)

	// Transform a point (pbrt 2.8.1)
	def *(p: Point) = {
		val w = 1.0 / (elems(12) * p.x + elems(13) * p.y + elems(14) * p.z + elems(15))
		new Point(
			(elems(0) * p.x + elems(1) * p.y + elems(2) * p.z + elems(3)) * w,
			(elems(4) * p.x + elems(5) * p.y + elems(6) * p.z + elems(7)) * w,
			(elems(8) * p.x + elems(9) * p.y + elems(10) * p.z + elems(11)) * w)
	}

	// Transform a vector (pbrt 2.8.2)
	def *(v: Vector) = new Vector(
		elems(0) * v.x + elems(1) * v.y + elems(2) * v.z,
		elems(4) * v.x + elems(5) * v.y + elems(6) * v.z,
		elems(8) * v.x + elems(9) * v.y + elems(10) * v.z)

	// Transform a normal (pbrt 2.8.3)
	def *(n: Normal) = new Normal(
		elems(0) * n.x + elems(4) * n.y + elems(8) * n.z,
		elems(1) * n.x + elems(5) * n.y + elems(9) * n.z,
		elems(2) * n.x + elems(6) * n.y + elems(10) * n.z)

	// Transform a ray (pbrt 2.8.4)
	def *(r: Ray): Ray = new Ray(this * r.origin, this * r.direction, r.minDistance, r.maxDistance)

	// Multiply with another matrix
	def *(m: Matrix) = new Matrix(
		for (row <- 0 to 3; col <- 0 to 3) yield this(row, 0) * m(0, col) + this(row, 1) * m(1, col) + this(row, 2) * m(2, col) + this(row, 3) * m(3, col))

	// Transpose
	def transpose = new Matrix(for (row <- 0 to 3; col <- 0 to 3) yield this(col, row))

	// Inverse
	def inverse = {
		def cofactor(i: Int, j: Int) = {
			def sub(row: Int, col: Int) = this(if (row < i) row else row + 1, if (col < j) col else col + 1)

			(if ((i + j) % 2 == 0) 1.0 else -1.0) * (
				sub(0, 0) * sub(1, 1) * sub(2, 2) + sub(0, 1) * sub(1, 2) * sub(2, 0) + sub(0, 2) * sub(1, 0) * sub(2, 1) -
				sub(0, 0) * sub(1, 2) * sub(2, 1) - sub(0, 1) * sub(1, 0) * sub(2, 2) - sub(0, 2) * sub(1, 1) * sub(2, 0))
		}

		val adjugate = for (i <- 0 to 3; j <- 0 to 3) yield cofactor(j, i)

		val determinant = elems(0) * adjugate(0) + elems(1) * adjugate(4) + elems(2) * adjugate(8) + elems(3) * adjugate(12)
		require(math.abs(determinant) > 1e-9, "Singular matrix")

		new Matrix(adjugate map (_ / determinant))
	}

	override def toString = "Matrix(%s)" format (elems.mkString(", "))
}

private object Matrix {
	// Matrix constants
	val Identity: Matrix = new Matrix(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0) {
		override def *(p: Point) = p
		override def *(v: Vector) = v
		override def *(n: Normal) = n
		override def *(r: Ray) = r
		override def *(m: Matrix) = m
		override def transpose = this
		override def inverse = this

		override def toString = "Matrix.Identity"
	}

	// Create a matrix
	def apply(elems: IndexedSeq[Double]) = new Matrix(elems)

	// Create a matrix (using varargs)
	def apply(elems: Double*) = new Matrix(elems: _*)
}
