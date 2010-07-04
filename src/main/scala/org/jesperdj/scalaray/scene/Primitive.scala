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

// Primitive (pbrt 4.1)
abstract class Primitive {
	// Bounding box in world coordinates (pbrt 4.1)
	val worldBound: BoundingBox

	// True if this primitive can be intersected, false if it needs to be refined (pbrt 4.1)
	val canIntersect: Boolean = true

	// Refine this primitive into a collection of parts (pbrt 4.1)
	def refine: Traversable[Primitive] = throw new java.lang.UnsupportedOperationException("This primitive cannot be refined")

	// Recursively refine this primitive into intersectable primitives (pbrt 4.1)
	def fullyRefine: Traversable[Primitive] = if (canIntersect) Traversable(this) else refine flatMap (_.fullyRefine)

	// Compute intersection between a ray and this primitive (pbrt 4.1)
	def intersect(ray: Ray): Option[Intersection]
}
