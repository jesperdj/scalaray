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

// TriangleMesh (pbrt 3.6)
final class TriangleMesh (val objectToWorld: Transform) extends Shape {
	// Bounding box in object coordinates (pbrt TODO)
	val objectBound: BoundingBox = BoundingBox.Empty // TODO

	// Compute intersection between a ray and this shape (pbrt TODO)
	def intersect(ray: Ray): Option[(DifferentialGeometry, Double)] = None // TODO

	// Surface area (pbrt TODO)
	val surfaceArea: Double = 0.0 // TODO

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = (null, null, 0.0) // TODO

	override def toString = "TriangleMesh(objectToWorld=%s)" format (objectToWorld)
}
