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
import org.jesperdj.scalaray.vecmath._

// Cylinder (pbrt 3.3)
final class Cylinder (val objectToWorld: Transform, radius: Double, z0: Double, z1: Double, pm: Double = 2.0 * π) extends Quadric {
	// Minimum and maximum z coordinates (pbrt 3.3.1)
	private val (minZ, maxZ) = minmax(z0, z1)

	// Maximum φ angle (pbrt 3.3.1)
	private val maxPhi = clamp(pm, 0.0, 2.0 * π)

	// Bounding box in object coordinates (pbrt 3.3.2)
	val objectBound = new BoundingBox(Point(-radius, -radius, minZ), Point(radius, radius, maxZ))

	// Compute quadratic coefficients (pbrt 3.3.3)
	protected def computeCoefficients(r: Ray): (Double, Double, Double) =
		(r.direction.x * r.direction.x + r.direction.y * r.direction.y,
		 2.0 * (r.direction.x * r.origin.x + r.direction.y * r.origin.y),
		 r.origin.x * r.origin.x + r.origin.y * r.origin.y - radius * radius)

	// Get differential geometry for an intersection point (pbrt 3.3.4)
	protected def differentialGeometry(p: Point): Option[DifferentialGeometry] = {
		// Check z range
		if (p.z < minZ || p.z > maxZ) return None

		// Check against max φ
		val phi = { val f = math.atan2(p.y, p.x); if (f >= 0.0) f else f + 2.0 * π }
		if (phi > maxPhi) return None

		// Initialize differential geometry (note: lazy evaluation to avoid unnecessary computations)
		Some(new DifferentialGeometry {
			// Intersection point
			lazy val point = objectToWorld * p

			// Surface normal (better method than what's used in pbrt)
			override lazy val normal = (objectToWorld * Normal(p.x, p.y, 0.0)).normalize

			// Surface parameter coordinates (pbrt 3.3.4)
			lazy val u = phi / maxPhi
			lazy val v = (p.z - minZ) / (maxZ - minZ)

			// Partial derivatives of the surface position and normal (pbrt 3.3.4)
			lazy val (dpdu, dpdv, dndu, dndv) = {
				val dpduObj = Vector(-maxPhi * p.y, maxPhi * p.x, 0.0)
				val dpdvObj = Vector(0.0, 0.0, maxZ - minZ)

				// TODO: Computations can be simplified because d2Pduv == d2Pdvv == Vector(0, 0, 0)
				val d2Pduu = (-maxPhi * maxPhi) * Vector(p.x, p.y, 0.0)
				val d2Pduv = Vector(0.0, 0.0, 0.0)
				val d2Pdvv = Vector(0.0, 0.0, 0.0)

				val (ee, ff, gg) = (dpduObj * dpduObj, dpduObj * dpdvObj, dpdvObj * dpdvObj)
				val n = (dpduObj ** dpdvObj).normalize
				val (e, f, g) = (n * d2Pduu, n * d2Pduv, n * d2Pdvv)

				val i = 1.0 / (ee * gg - ff * ff)

				val dnduObj = Normal(((f * ff - e * gg) * i) * dpduObj + ((e * ff - f * ee) * i) * dpdvObj)
				val dndvObj = Normal(((g * ff - f * gg) * i) * dpduObj + ((f * ff - g * ee) * i) * dpdvObj)

				// Transform vectors from object to world coordinates
				(objectToWorld * dpduObj, objectToWorld * dpdvObj,
				 objectToWorld * dnduObj, objectToWorld * dndvObj)
			}

			// Shape which is intersected
			val shape = Cylinder.this
		})
	}

	// Surface area (pbrt 3.3.5)
	val surfaceArea = maxPhi * radius * (maxZ - minZ)

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = {
		val phi = u2 * maxPhi
		val p = new Point(radius * math.cos(phi), radius * math.sin(phi), interpolate(u1, minZ, maxZ))
		val n = (objectToWorld * Normal(p.x, p.y, 0.0)).normalize
		(objectToWorld * p, n, 1.0 / surfaceArea)
	}

	override def toString = "Cylinder(objectToWorld=%s, radius=%g, minZ=%g, maxZ=%g, maxPhi=%g)" format (objectToWorld, radius, minZ, maxZ, maxPhi)
}
