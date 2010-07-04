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

import scala.collection.immutable.Traversable

import org.jesperdj.scalaray.vecmath._

// Shape (pbrt 3.1)
abstract class Shape {
	// Transformation from object to world coordinates
	val objectToWorld: Transform

	// Transformation from world to object coordinates
	lazy val worldToObject: Transform = objectToWorld.inverse

	// Bounding box in object coordinates (pbrt 3.1.1)
	val objectBound: BoundingBox

	// Bounding box in world coordinates (override this if shape can supply a tighter world bound) (pbrt 3.1.1)
	lazy val worldBound: BoundingBox = objectToWorld * objectBound

	// True if this shape can be intersected, false if it needs to be refined (pbrt 3.1.2)
	val canIntersect: Boolean = true

	// Refine this shape into a collection of parts (pbrt 3.1.2)
	def refine: Traversable[Shape] = throw new java.lang.UnsupportedOperationException("This shape cannot be refined")

	// Compute intersection between a ray and this shape, returns differential geometry and distance of intersection along ray (pbrt 3.1.3)
	def intersect(ray: Ray): Option[(DifferentialGeometry, Double)]

	// Compute shading geometry (pbrt 3.1.5)
	def shadingGeometry(objToWorld: Transform, dg: DifferentialGeometry): DifferentialGeometry = dg

	// Surface area (pbrt 3.1.6)
	def surfaceArea: Double

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double)

	// Sample a point on the surface with respect to a point from which the shape is visible using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(point: Point, u1: Double, u2: Double): (Point, Normal, Double) = sampleSurface(u1, u2)
}
