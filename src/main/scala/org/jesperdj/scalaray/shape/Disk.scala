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
final class Disk (radius: Double = 1.0, innerRadius: Double = 0.0, maxPhi: Double = 2.0 * π) extends Shape {
	require(radius > 0.0, "radius must be > 0")
	require(innerRadius >= 0.0, "innerRadius must be >= 0")
	require(innerRadius < radius, "innerRadius must be < radius")
	require(maxPhi >= 0.0 && maxPhi <= 2.0 * π, "maxPhi must be >= 0 and <= 2π")

	// Bounding box that contains the shape (pbrt 3.4.2)
	val boundingBox: BoundingBox = BoundingBox(Point(-radius, -radius, 0.0), Point(radius, radius, 0.0))

	// Compute intersection between a ray and this shape, returns differential geometry and distance of intersection along ray (pbrt 3.4.3)
	def intersect(ray: Ray): Option[(DifferentialGeometry, Double)] = {
		// Check if ray is (almost) parallel to the plane of the disk
		if (math.abs(ray.direction.z) < 1e-9) return None

		// Compute distance, check if in range of the ray
		val distance = -ray.origin.z / ray.direction.z
		if (!ray.isInRange(distance)) return None

		// Compute intersection point, check if it is inside the disk radii
		val p = ray.point(distance)
		val distanceSquared = p.x * p.x + p.y * p.y
		if (distanceSquared > radius * radius || distanceSquared < innerRadius * innerRadius) return None

		// Check against max φ
		val phi = { val f = math.atan2(p.y, p.x); if (f >= 0.0) f else f + 2.0 * π }
		if (phi > maxPhi) return None

		// Initialize differential geometry
		Some(new DifferentialGeometry {
			// Intersection point
			val point: Point = p

			// Surface normal
			val normal: Normal = Normal.ZAxis

			// Surface parameter coordinates (pbrt 3.4.3)
			lazy val u: Double = phi / maxPhi
			lazy val v: Double = 1.0 - ((math.sqrt(distanceSquared) - innerRadius) / (radius - innerRadius))

			// Shape which is intersected
			val shape: Shape = Disk.this
		}, distance)
	}

	// Surface area (pbrt 3.4.4)
	val surfaceArea: Double = 0.5 * maxPhi * (radius * radius - innerRadius * innerRadius)

	// Sample a point on the surface using the random variables u1, u2 (pbrt 15.6.3)
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = {
		val (x, y) = SampleTransforms.concentricSampleDisk(u1, u2)
		(Point(x * radius, y * radius, 0.0), Normal.ZAxis, 1.0 / surfaceArea)
		// TODO: We are not taking partial disks into account (innerRadius and maxPhi). See pbrt exercise 15.1 (page 716).
	}

	override def toString = "Disk(radius=%g, innerRadius=%g, maxPhi=%g)" format (radius, innerRadius, maxPhi)
}
