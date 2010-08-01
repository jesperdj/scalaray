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

import org.jesperdj.scalaray.sampler.SampleTransforms
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// Sphere (pbrt 3.2)
final class Sphere (radius: Float = 1.0f, minZ: Float = Float.NegativeInfinity, maxZ: Float = Float.PositiveInfinity, maxPhi: Float = 2.0f * π) extends Quadric {
	require(radius > 0.0f, "radius must be > 0")
	require(minZ < maxZ, "minZ must be < maxZ")
	require(maxPhi >= 0.0f && maxPhi <= 2.0f * π, "maxPhi must be >= 0 and <= 2π")

	// Minimum and maximum θ angle (pbrt 3.2.1)
	private val minTheta = math.acos(clamp(minZ / radius, -1.0f, 1.0f)).toFloat
	private val maxTheta = math.acos(clamp(maxZ / radius, -1.0f, 1.0f)).toFloat
	private val diffTheta = maxTheta - minTheta

	// Bounding box that contains the shape (pbrt 3.2.2)
	val boundingBox: BoundingBox = BoundingBox(Point(-radius, -radius, minZ), Point(radius, radius, maxZ))

	// Compute quadratic coefficients (pbrt 3.2.3)
	protected def computeCoefficients(ray: Ray): (Float, Float, Float) =
		(ray.direction.x * ray.direction.x + ray.direction.y * ray.direction.y + ray.direction.z * ray.direction.z,
		 2.0f * (ray.direction.x * ray.origin.x + ray.direction.y * ray.origin.y + ray.direction.z * ray.origin.z),
		 ray.origin.x * ray.origin.x + ray.origin.y * ray.origin.y + ray.origin.z * ray.origin.z - radius * radius)

	// Get differential geometry for an intersection point (pbrt 3.2.4, 3.2.5, 3.2.6)
	protected def differentialGeometry(p: Point): Option[DifferentialGeometry] = {
		// Check z range
		if (p.z < minZ || p.z > maxZ) return None

		// Check against max φ
		val phi = { val f = math.atan2(p.y, p.x).toFloat; if (f >= 0.0f) f else f + 2.0f * π }
		if (phi > maxPhi) return None

		// Initialize differential geometry
		Some(new DifferentialGeometry {
			// Intersection point
			val point: Point = p

			// Surface normal (better method than what's used in pbrt)
			lazy val normal: Normal = Normal(p).normalize

			private lazy val theta = math.acos(clamp(p.z / radius, -1.0f, 1.0f)).toFloat

			// Surface parameter coordinates (pbrt 3.3.4)
			lazy val u: Float = phi / maxPhi
			lazy val v: Float = (theta - minTheta) / diffTheta

			// Partial derivatives of the surface position and normal
			lazy val (dpdu, dpdv, dndu, dndv): (Vector, Vector, Normal, Normal) = {
				val radiusZ = math.sqrt(p.x * p.x + p.y * p.y).toFloat
				val (cosPhi, sinPhi) = if (radiusZ > 0.0f) (p.x / radiusZ, p.y / radiusZ) else (0.0f, 1.0f)

				val dpdv = Vector(cosPhi * p.z, sinPhi * p.z, -radius * math.sin(theta).toFloat) * diffTheta
				val dpdu = if (radiusZ > 0.0f) Vector(-maxPhi * p.y, maxPhi * p.x, 0.0f) else dpdv ** Vector(p)

				val d2Pduu = Vector(p.x, p.y, 0.0f) * (-maxPhi * maxPhi)
				val d2Pduv = Vector(-sinPhi, cosPhi, 0.0f) * (diffTheta * maxPhi * p.z)
				val d2Pdvv = Vector(p) * (-diffTheta * diffTheta)

				val E = dpdu * dpdu; val F = dpdu * dpdv; val G = dpdv * dpdv
				val N = (dpdu ** dpdv).normalize
				val e = N * d2Pduu; val f = N * d2Pduv; val g = N * d2Pdvv

				val EGF2 = (E * G - F * F)
				val dndu = Normal(dpdu * ((f * F - e * G) / EGF2) + dpdv * ((e * F - f * E) / EGF2))
				val dndv = Normal(dpdu * ((g * F - f * G) / EGF2) + dpdv * ((f * F - g * E) / EGF2))

				(dpdu, dpdv, dndu, dndv)
			}

			// Shape which is intersected
			val shape: Shape = Sphere.this
		})
	}

	// Surface area (pbrt 3.2.7)
	val surfaceArea: Float = maxPhi * radius * (maxZ - minZ)

	// Sample a point on the surface using the random variables u1, u2 (pbrt 14.6.3)
	// Returns a point on the surface, the surface normal at that point and the probability density for this sample
	def sampleSurface(u1: Float, u2: Float): (Point, Normal, Float) = {
		val p = SampleTransforms.uniformSampleSphere(u1, u2)
		(p * radius, Normal(p), 1.0f / surfaceArea)
		// TODO: We are not taking partial spheres into account (innerRadius and maxPhi). See pbrt exercise 14.3 (page 734).
	}

	// Sample a point on the surface with respect to a point from which the shape is visible using the random variables u1, u2 (pbrt 14.6.3)
	// Returns a point on the surface, the surface normal at that point and the probability density for this sample
	override def sampleSurface(viewPoint: Point, u1: Float, u2: Float): (Point, Normal, Float) =
		throw new UnsupportedOperationException("Not yet implemented") // TODO: Implement this; see pbrt 14.6.3 (page 720-722)

	// Probability density at the given point on the surface with respect to the distribution that sampleSurface(viewPoint: Point, u1: Float, u2: Float)
	// uses to sample points (pbrt 14.6.3)
	override def pdf(viewPoint: Point, wi: Vector): Float =
		throw new UnsupportedOperationException("Not yet implemented") // TODO: Implement this; see pbrt 14.6.3 (page 720-722)

	override def toString = "Sphere(radius=%g, minZ=%g, maxZ=%g, maxPhi=%g)" format (radius, minZ, maxZ, maxPhi)
}
