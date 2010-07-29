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
	val boundingBox: BoundingBox = (BoundingBox.Empty /: primitives) { (bb, p) => bb union p.boundingBox }

	// Bounding box when primitive is transformed
	override def boundingBox(transform: Transform): BoundingBox = (BoundingBox.Empty /: primitives) { (bb, p) => bb union p.boundingBox(transform) }

	// Compute closest intersection between a ray and this primitive, returns intersection and and distance of intersection along ray
	def intersect(ray: Ray): Option[(Intersection, Float)] = {
		// Check against bounding box
		val range = boundingBox intersect ray
		if (range.isEmpty) return None

		// Initialize the range of the ray with the range inside the bounding box
		val (minDistance, maxDistance) = range.get
		var r = Ray(ray.origin, ray.direction, minDistance, maxDistance)

		// Find the closest intersection between the ray and one of the primitives
		((None: Option[(Intersection, Float)]) /: primitives) { (result: Option[(Intersection, Float)], prim: Primitive) =>
			prim intersect r match {
				case Some((its, distance)) =>
					// Found a closer intersection; update max distance of the ray for subsequent intersection tests
					r = Ray(r.origin, r.direction, r.minDistance, distance)
					Some(its, distance)

				case None => result
			}
		}
	}

	// Check if a ray intersects this primitive
	override def checkIntersect(ray: Ray): Boolean = primitives exists (_ checkIntersect ray)

	override def toString = "CompositePrimitive(primitives=%s)" format (primitives)
}
