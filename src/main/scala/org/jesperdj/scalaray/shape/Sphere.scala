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
final class Sphere (val objectToWorld: Transform, radius: Double,
					z0: Double = Double.NegativeInfinity, z1: Double = Double.PositiveInfinity, pm: Double = 2.0 * π) extends Quadric {
	// Minimum and maximum z coordinates (pbrt 3.2.1)
	private val minZ = clamp(math.min(z0, z1), -radius, radius)
	private val maxZ = clamp(math.max(z0, z1), -radius, radius)

	// Maximum φ angle (pbrt 3.2.1)
	private val maxPhi = clamp(pm, 0.0, 2.0 * π)

	// Minimum and maximum θ angle (pbrt 3.2.1)
	private val minTheta = math.acos(clamp(minZ / radius, -1.0, 1.0))
	private val maxTheta = math.acos(clamp(maxZ / radius, -1.0, 1.0))
	private val diffTheta = maxTheta - minTheta

	// Bounding box in object coordinates (pbrt 3.2.2)
	val objectBound = new BoundingBox(Point(-radius, -radius, minZ), Point(radius, radius, maxZ))

	// Compute quadratic coefficients (pbrt 3.2.3)
	protected def computeCoefficients(r: Ray): (Double, Double, Double) =
		(r.direction.x * r.direction.x + r.direction.y * r.direction.y + r.direction.z * r.direction.z,
		 2.0 * (r.direction.x * r.origin.x + r.direction.y * r.origin.y + r.direction.z * r.origin.z),
		 r.origin.x * r.origin.x + r.origin.y * r.origin.y + r.origin.z * r.origin.z - radius * radius)

	// Get differential geometry for an intersection point (pbrt 3.2.4, 3.2.5, 3.2.6)
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

			// Surface normal (works also on the poles of the sphere; the version in DifferentialGeometry does not)
			override lazy val normal = (objectToWorld * Normal(p)).normalize

			// Surface parameter coordinates (pbrt 3.2.4)
			lazy val u = phi / maxPhi
			lazy val v = (math.acos(p.z / radius) - minTheta) / diffTheta

			// Partial derivatives of the surface position and normal (pbrt 3.2.4, 3.2.5, 3.2.6)
			lazy val (dpdu, dpdv, dndu, dndv) = {
				// TODO: Handle the poles of the sphere (where p.x == p.y == 0 so that zr == 0) as special cases to avoid NaNs

				val zr = math.sqrt(p.x * p.x + p.y * p.y)
				val (sinPhi, cosPhi) = (p.x / zr, p.y / zr)

				val dpduObj = Vector(-maxPhi * p.y, maxPhi * p.x, 0.0)
				val dpdvObj = diffTheta * Vector(p.z * cosPhi, p.z * sinPhi, -radius * math.sin(minTheta + v * diffTheta))

				val d2Pduu = (-maxPhi * maxPhi) * Vector(p.x, p.y, 0.0)
				val d2Pduv = (diffTheta * p.z * maxPhi) * Vector(-sinPhi, cosPhi, 0.0)
				val d2Pdvv = (-diffTheta * diffTheta) * Vector(p)

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
			val shape = Sphere.this
		})
	}

	// Surface area (pbrt 3.2.7)
	val surfaceArea = maxPhi * radius * (maxZ - minZ)

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = {
		val p = Point.Origin + new Vector(SampleTransforms.uniformSampleSphere(u1, u2)) * radius
		val n = (objectToWorld * Normal(p)).normalize
		(objectToWorld * p, n, 1.0 / surfaceArea)
	}
	//
	// Sample a point on the surface with respect to a point from which the shape is visible using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	override def sampleSurface(point: Point, u1: Double, u2: Double): (Point, Normal, Double) =
		throw new UnsupportedOperationException("Not yet implemented") // TODO: zie pbrt pagina 705-708

	override def toString = "Sphere(objectToWorld=%s, radius=%g, minZ=%g, maxZ=%g, maxPhi=%g)" format (objectToWorld, radius, minZ, maxZ, maxPhi)
}
