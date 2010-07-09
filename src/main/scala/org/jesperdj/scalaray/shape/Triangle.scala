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
package org.jesperdj.scalaray.shape

import org.jesperdj.scalaray.vecmath._

// Vertex
final class Vertex (val point: Point, val normal: Normal, val u: Double, val v: Double) {
	override def toString = "Vertex(point=%s, normal=%s, u=%g, v=%g)" format (point, normal, u, v)
}

// Triangle
final class Triangle (v1: Vertex, v2: Vertex, v3: Vertex) extends Shape {
	// Edge vectors
	private val e1: Vector = v2.point - v1.point
	private val e2: Vector = v3.point - v1.point

	// Bounding box that contains the object
	val boundingBox: BoundingBox = BoundingBox(v1.point, v2.point, v3.point)

	// Bounding box when object is transformed
	override def boundingBox(transform: Transform): BoundingBox = BoundingBox(transform * v1.point, transform * v2.point, transform * v3.point)

	// Compute intersection between a ray and this shape, returns differential geometry and distance of intersection along ray
	def intersect(ray: Ray): Option[(DifferentialGeometry, Double)] =
		throw new UnsupportedOperationException("Not yet implemented") // TODO

	// Surface area
	val surfaceArea: Double = 0.0 // TODO

	// Sample a point on the surface using the random variables u1, u2
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) =
		throw new UnsupportedOperationException("Not yet implemented") // TODO

	override def toString = "Triangle(%s, %s, %s)" format (v1, v2, v3)
}
