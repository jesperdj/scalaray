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

import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// Cylinder (pbrt 3.3)
final class Cylinder (radius: Double = 1.0, minZ: Double = -1.0, maxZ: Double = 1.0, maxPhi: Double = 2.0 * π) extends Quadric {
	require(radius > 0.0, "radius must be > 0")
	require(minZ < maxZ, "minZ must be < maxZ")
	require(maxPhi >= 0.0 && maxPhi <= 2.0 * π, "maxPhi must be >= 0 and <= 2π")

	// Bounding box that contains the shape (pbrt 3.3.2)
	val boundingBox: BoundingBox = BoundingBox(Point(-radius, -radius, minZ), Point(radius, radius, maxZ))

	// Compute quadratic coefficients (pbrt 3.3.3)
	protected def computeCoefficients(ray: Ray): (Double, Double, Double) =
		(ray.direction.x * ray.direction.x + ray.direction.y * ray.direction.y,
		 2.0 * (ray.direction.x * ray.origin.x + ray.direction.y * ray.origin.y),
		 ray.origin.x * ray.origin.x + ray.origin.y * ray.origin.y - radius * radius)

	// Get differential geometry for an intersection point (pbrt 3.3.4)
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
			lazy val normal: Normal = Normal(p.x, p.y, 0.0).normalize

			// Surface parameter coordinates (pbrt 3.3.4)
			lazy val u: Double = phi / maxPhi
			lazy val v: Double = (p.z - minZ) / (maxZ - minZ)

			// Partial derivatives of the surface position
			lazy val dpdu: Vector = Vector(-maxPhi * p.y, maxPhi * p.x, 0.0)
			lazy val dpdv: Vector = Vector(0.0, 0.0, maxZ - minZ)

			// Partial derivatives of the surface normal
			lazy val (dndu, dndv): (Normal, Normal) = {
				val d2Pduu = Vector(p.x, p.y, 0.0) * (-maxPhi * maxPhi)
				val d2Pduv = Vector.Zero
				val d2Pdvv = Vector.Zero

				// TODO: This can be simplified because d2Pduv and d2Pdvv are Vector.Zero for a cylinder

				val E = dpdu * dpdu; val F = dpdu * dpdv; val G = dpdv * dpdv
				val N = (dpdu ** dpdv).normalize
				val e = N * d2Pduu; val f = N * d2Pduv; val g = N * d2Pdvv

				val EGF2 = (E * G - F * F)
				val dndu = Normal(dpdu * ((f * F - e * G) / EGF2) + dpdv * ((e * F - f * E) / EGF2))
				val dndv = Normal(dpdu * ((g * F - f * G) / EGF2) + dpdv * ((f * F - g * E) / EGF2))

				(dndu, dndv)
			}

			// Shape which is intersected
			val shape: Shape = Cylinder.this
		})
	}

	// Surface area (pbrt 3.3.5)
	val surfaceArea: Double = maxPhi * radius * (maxZ - minZ)

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = {
		val phi = u2 * maxPhi
		val (x, y) = (math.cos(phi), math.sin(phi))
		(Point(x * radius, y * radius, interpolate(u1, minZ, maxZ)), Normal(x, y, 0.0), 1.0 / surfaceArea)
	}

	override def toString = "Cylinder(radius=%g, minZ=%g, maxZ=%g, maxPhi=%g)" format (radius, minZ, maxZ, maxPhi)
}
