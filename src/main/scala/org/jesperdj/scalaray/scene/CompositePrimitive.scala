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
package org.jesperdj.scalaray.scene

import scala.collection.immutable.Traversable

import org.jesperdj.scalaray.shape.BoundingBox
import org.jesperdj.scalaray.vecmath._

// Composite primitive: contains a collection of primitives
final class CompositePrimitive (primitives: Traversable[Primitive]) extends Primitive {
	require(primitives.size >= 2, "A CompositePrimitive must contain at least two primitives")

	// Create a composite primitive using varargs
	def this(primitives: Primitive*) = this(Traversable(primitives: _*))

	// Bounding box that contains the primitive
	val boundingBox: BoundingBox = primitives.foldLeft(BoundingBox.Empty) { (bb, p) => bb union p.boundingBox }

	// Bounding box when primitive is transformed
	override def boundingBox(transform: Transform): BoundingBox = primitives.foldLeft(BoundingBox.Empty) { (bb, p) => bb union p.boundingBox(transform) }

	// Compute intersection between a ray and this primitive
	def intersect(ray: Ray): Option[Intersection] = {
		// Check against bounding box
		val bi = boundingBox intersect ray
		if (bi.isEmpty) return None

		// Initialize the range of the ray with the range inside the bounding box
		val (minT, maxT) = bi.get
		var r = Ray(ray.origin, ray.direction, minT, maxT)

		// Find the closest intersection between the ray and one of the primitives
		primitives.foldLeft(None: Option[Intersection]) { (o: Option[Intersection], p: Primitive) =>
			val pi = p intersect r
			if (pi.isEmpty) o else { r = Ray(r.origin, r.direction, r.minDistance, pi.get.distance); pi }
		}
	}

	override def toString = "CompositePrimitive(primitives=%s)" format (primitives)
}
