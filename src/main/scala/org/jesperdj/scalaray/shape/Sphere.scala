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

import org.jesperdj.scalaray._
import org.jesperdj.scalaray.sampler.SampleTransforms
import org.jesperdj.scalaray.vecmath._

// Sphere (pbrt 3.2)
final class Sphere (radius: Double = 1.0, minZ: Double = Double.NegativeInfinity, maxZ: Double = Double.PositiveInfinity, maxPhi: Double = 2.0 * π) extends Quadric {
	require(radius > 0.0, "radius must be > 0")
	require(minZ < maxZ, "minZ must be < maxZ")
	require(maxPhi >= 0.0 && maxPhi <= 2.0 * π, "maxPhi must be >= 0 and <= 2π")

	// Minimum and maximum θ angle (pbrt 3.2.1)
	private val minTheta = math.acos(clamp(minZ / radius, -1.0, 1.0))
	private val maxTheta = math.acos(clamp(maxZ / radius, -1.0, 1.0))
	private val diffTheta = maxTheta - minTheta

	// Bounding box that contains the shape (pbrt 3.2.2)
	val boundingBox: BoundingBox = BoundingBox(Point(-radius, -radius, minZ), Point(radius, radius, maxZ))

	// Compute quadratic coefficients (pbrt 3.2.3)
	protected def computeCoefficients(ray: Ray): (Double, Double, Double) =
		(ray.direction.x * ray.direction.x + ray.direction.y * ray.direction.y + ray.direction.z * ray.direction.z,
		 2.0 * (ray.direction.x * ray.origin.x + ray.direction.y * ray.origin.y + ray.direction.z * ray.origin.z),
		 ray.origin.x * ray.origin.x + ray.origin.y * ray.origin.y + ray.origin.z * ray.origin.z - radius * radius)

	// Get differential geometry for an intersection point (pbrt 3.2.4, 3.2.5, 3.2.6)
	protected def differentialGeometry(p: Point): Option[DifferentialGeometry] = {
		// Check z range
		if (p.z < minZ || p.z > maxZ) return None

		// Check against max φ
		val phi = { val f = math.atan2(p.y, p.x); if (f >= 0.0) f else f + 2.0 * π }
		if (phi > maxPhi) return None

		// Initialize differential geometry
		Some(new DifferentialGeometry {
			// Intersection point
			val point: Point = p

			// Surface normal (better method than what's used in pbrt)
			lazy val normal: Normal = Normal(p).normalize

			// Surface parameter coordinates (pbrt 3.3.4)
			lazy val u: Double = phi / maxPhi
			lazy val v: Double = (math.acos(p.z / radius) - minTheta) / diffTheta

			// Shape which is intersected
			val shape: Shape = Sphere.this
		})
	}

	// Surface area (pbrt 3.2.7)
	val surfaceArea: Double = maxPhi * radius * (maxZ - minZ)

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = {
		val p = SampleTransforms.uniformSampleSphere(u1, u2)
		(p * radius, Normal(p), 1.0 / surfaceArea)
		// TODO: We are not taking partial spheres into account (innerRadius and maxPhi). See pbrt exercise 15.1 (page 716).
	}

	// Sample a point on the surface with respect to a point from which the shape is visible using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	override def sampleSurface(point: Point, u1: Double, u2: Double): (Point, Normal, Double) =
		throw new UnsupportedOperationException("Not yet implemented") // TODO: See pbrt book page 705-708

	override def toString = "Sphere(radius=%g, minZ=%g, maxZ=%g, maxPhi=%g)" format (radius, minZ, maxZ, maxPhi)
}
