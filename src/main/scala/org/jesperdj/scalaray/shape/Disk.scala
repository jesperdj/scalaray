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

// Disk (pbrt 3.4)
final class Disk (val objectToWorld: Transform, height: Double, radius: Double, innerRadius: Double = 0.0, pm: Double = 2.0 * π) extends Shape {
	// Maximum φ angle (pbrt 3.4.1)
	private val maxPhi = clamp(pm, 0.0, 2.0 * π)

	// Bounding box in object coordinates (pbrt 3.4.2)
	val objectBound = new BoundingBox(Point(-radius, -radius, height - 1e-9), Point(radius, radius, height + 1e-9))

	// Compute intersection between a ray and this shape (pbrt 3.4.3)
	def intersect(ray: Ray): Option[(DifferentialGeometry, Double)] = {
		// Transform ray to object coordinates
		val r = worldToObject * ray

		// Check if ray is (almost) parallel to the plane of the disk
		if (math.abs(r.direction.z) < 1e-9) return None

		// Compute distance, check if in range of the ray
		val t = (height - r.origin.z) / r.direction.z
		if (!r.isInRange(t)) return None

		// Compute intersection point, check if it is inside the disk radii
		val p = r.point(t)
		val distanceSquared = p.x * p.x + p.y * p.y
		if (distanceSquared > radius * radius || distanceSquared < innerRadius * innerRadius) return None

		// Check against max φ
		val phi = { val f = math.atan2(p.y, p.x); if (f >= 0.0) f else f + 2.0 * π }
		if (phi > maxPhi) return None

		// Initialize differential geometry (note: lazy evaluation to avoid unnecessary computations)
		Some(new DifferentialGeometry {
			// Intersection point
			lazy val point = objectToWorld * p

			// Surface normal (fixed to Z axis in object coordinates)
			override lazy val normal = (objectToWorld * Normal.ZAxis).normalize

			// Surface parameter coordinates (pbrt 3.4.3)
			lazy val u = phi / maxPhi
			lazy val v = 1.0 - ((math.sqrt(distanceSquared) - innerRadius) / (radius - innerRadius))

			// Partial derivatives of the surface position with respect to the (u, v) coordinates
			lazy val dpdu: Vector = objectToWorld * (Vector(-maxPhi * p.y, maxPhi * p.x, 0.0) * (maxPhi / (2.0 * π)))
			lazy val dpdv: Vector = objectToWorld * (Vector(-p.x / (1.0 - v), -p.y / (1.0 - v), 0.0) * ((radius - innerRadius) / radius))

			// Partial derivatives of the surface normal with respect to the (u, v) coordinates
			val dndu: Normal = Normal.Zero
			val dndv: Normal = Normal.Zero

			// Shape which is intersected
			val shape = Disk.this
		}, t)
	}

	// Surface area (pbrt 3.4.4)
	val surfaceArea: Double = 0.5 * maxPhi * (radius * radius - innerRadius * innerRadius)

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = {
		val (x, y) = SampleTransforms.concentricSampleDisk(u1, u2)
		val p = new Point(x * radius, y * radius, height)
		val n = (objectToWorld * Normal.ZAxis).normalize
		(objectToWorld * p, n, 1.0 / surfaceArea)
	}

	override def toString =
		"Disk(objectToWorld=%s, height=%g, radius=%g, innerRadius=%g, maxPhi=%g)" format (objectToWorld, height, radius, innerRadius, maxPhi)
}
