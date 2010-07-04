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

import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.vecmath._

// Aggregate primitive
final class AggregatePrimitive (primitives: Traversable[Primitive]) extends Primitive {
	require(primitives.size >= 2, "An AggregatePrimitive must contain at least two primitives")
	require(primitives forall (_.canIntersect), "AggregatePrimitive requires that all its primitives are intersectable")

	// Create an aggregate primitive (using varargs)
	def this(primitives: Primitive*) = this(Traversable(primitives: _*))

	// Bounding box in world coordinates
	val worldBound = primitives.foldLeft(BoundingBox.Empty) { (bb, p) => bb union p.worldBound }

	// Compute intersection between a ray and this primitive
	def intersect(ray: Ray): Option[Intersection] = {
		// First check against bounding box of the aggregate
		val bi = worldBound intersect ray
		if (bi.isEmpty) return None

		val (minT, maxT) = bi.get
		var r = Ray(ray.origin, ray.direction, minT, maxT)

		// Find closest intersection between the ray and one of the primitives
		val none: Option[Intersection] = None
		primitives.foldLeft(none) { (o: Option[Intersection], p: Primitive) =>
			val pi = p intersect r
			if (pi.isDefined) { r = Ray(r.origin, r.direction, r.minDistance, pi.get.distance); pi } else o
		}
	}

	override def toString = "AggregatePrimitive(primitives=%s)" format (primitives)
}
