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

// NOTE: ScalaRay doesn't implement the shape and primitive refinement functionality of pbrt (pbrt 3.1.2, 4.1).
// It is not necessary and only complicates the architecture.

// Shape, describes geometry of a surface (pbrt 3.1)
abstract class Shape extends HasBoundingBox {
	// Compute closest intersection between a ray and this shape, returns differential geometry and distance of intersection along ray (pbrt 3.1.3)
	def intersect(ray: Ray): Option[(DifferentialGeometry, Float)]

	// Check if a ray intersects this shape; override this if the shape can provide a more efficient implementation (pbrt 3.1.3)
	def checkIntersect(ray: Ray): Boolean = intersect(ray).isDefined

	// Get shading geometry (pbrt 3.1.5)
	def shadingGeometry(dg: DifferentialGeometry, objectToWorld: Transform): DifferentialGeometry = dg

	// Surface area
	def surfaceArea: Float

	// Sample a point on the surface using the random variables u1, u2 (pbrt 14.6.3)
	// Returns a point on the surface, the surface normal at that point and the probability density for this sample
	def sampleSurface(u1: Float, u2: Float): (Point, Normal, Float)

	// Probability density of the given point on the surface being sampled with respect to the distribution that sampleSurface(u1: Float, u2: Float)
	// uses to sample points (pbrt 14.6.3)
	// NOTE: This must be overriden if sampleSurface(u1: Float, u2: Float) does not sample the surface uniformly
	def pdf(point: Point): Float = 1.0f / surfaceArea

	// Sample a point on the surface with respect to a point from which the shape is visible using the random variables u1, u2 (pbrt 14.6.3)
	// Returns a point on the surface, the surface normal at that point and the probability density for this sample
	def sampleSurface(viewPoint: Point, u1: Float, u2: Float): (Point, Normal, Float) = sampleSurface(u1, u2)

	// Probability density of the direction wi (from viewPoint to a point on the surface) being sampled with respect to the distribution
	// that sampleSurface(viewPoint: Point, u1: Float, u2: Float) uses to sample points (pbrt 14.6.3)
	def pdf(viewPoint: Point, wi: Vector): Float = intersect(Ray(viewPoint, wi, 1e-3f)) match {
		case Some((dg, _)) => val f = (dg.normal * -wi).abs; if (f > 0.0f) viewPoint.distanceSquared(dg.point) / (f * surfaceArea) else 0.0f
		case None => 0.0f
	}
}
