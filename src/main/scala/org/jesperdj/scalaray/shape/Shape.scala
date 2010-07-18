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

// NOTE: In contrast to pbrt, shapes in ScalaRay do not have a shape-to-world transform. Transformations have been abstracted away to TransformedPrimitive.
// The methods in shapes work with and return values in local shape coordinates, not world coordinates as in pbrt.

// Shape, describes geometry of a surface
abstract class Shape extends HasBoundingBox {
	// Compute intersection between a ray and this shape, returns differential geometry and distance of intersection along ray
	def intersect(ray: Ray): Option[(DifferentialGeometry, Double)]

	// Surface area
	def surfaceArea: Double

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double)

	// Get the value of the probability density function at the given point on the surface with respect to
	// the distribution that sampleSurface(u1: Double, u2: Double) uses to sample points (pbrt 15.6.3)
	// NOTE: This must be overriden if sampleSurface(u1: Double, u2: Double) does not sample the surface uniformly
	def pdf(point: Point): Double = 1.0 / surfaceArea

	// Sample a point on the surface with respect to a point from which the shape is visible using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(viewPoint: Point, u1: Double, u2: Double): (Point, Normal, Double) = sampleSurface(u1, u2)
	
	// Get the value of the probability density function at the intersection point between the given ray and this shape with respect to
	// the distribution that sampleSurface(viewPoint: Point, u1: Double, u2: Double) uses to sample points (pbrt 15.6.3)
	def pdf(ray: Ray): Double = intersect(ray) match {
		case Some((dg, _)) =>
			val f = math.abs(dg.normal * -ray.direction)
			if (f > 0.0) ray.origin.distanceSquared(dg.point) / (f * surfaceArea) else 0.0

		case None => 0.0
	}
}
