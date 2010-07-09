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

// Point (pbrt 2.3)
final class Point (val x: Double, val y: Double, val z: Double) {
	// Create a point from a vector
	def this(v: Vector) = this(v.x, v.y, v.z)

	// Add a vector to a point (pbrt 2.3)
	def +(v: Vector) = new Point(x + v.x, y + v.y, z + v.z)

	// Add a normal to a point
	def +(n: Normal) = new Point(x + n.x, y + n.y, z + n.z)

	// Subtract a vector from a point (pbrt 2.3)
	def -(v: Vector) = new Point(x - v.x, y - v.y, z - v.z)

	// Subtract a normal from a point
	def -(n: Normal) = new Point(x - n.x, y - n.y, z - n.z)

	// Subtract two points (pbrt 2.3)
	def -(p: Point) = new Vector(x - p.x, y - p.y, z - p.z)

	// Multiply or divide a point by a weight (pbrt 2.3)
	def *(f: Double) = new Point(x * f, y * f, z * f)
	def /(f: Double) = new Point(x / f, y / f, z / f)

	// Distance between two points (pbrt 2.3)
	def distance(p: Point) = (this - p).length

	override def toString = "Point(%g, %g, %g)" format (x, y, z)
}

object Point {
	// Point constants
	val Origin = new Point(0.0, 0.0, 0.0)
	val PositiveInfinity = new Point(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
	val NegativeInfinity = new Point(Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity)

	// Create a point (pbrt 2.3)
	def apply(x: Double, y: Double, z: Double) = new Point(x, y, z)

	// Create a point from a vector
	def apply(v: Vector) = new Point(v)
}
