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

import org.jesperdj.scalaray.lightsource._
import org.jesperdj.scalaray.material._
import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.vecmath._

// TODO: Kan een geometric primitive met een area light source ook nog een material hebben?

// Geometric primitive, primitive that has a shape (pbrt 4.1.1)
final class GeometricPrimitive private (shape: Shape, material: Material, val areaLight: Option[AreaLightSource]) extends Primitive {
	// Create a geometric primitive with a shape and material, no area light source
	def this(shape: Shape, material: Material) = this(shape, material, None)

	// Create a geometric primitive with an area light source and material
	def this(areaLight: AreaLightSource, material: Material) = this(areaLight.shape, material, Some(areaLight))

	// Bounding box in world coordinates (pbrt 4.1.1)
	val worldBound = shape.worldBound

	// True if this primitive can be intersected, false if it needs to be refined (pbrt 4.1.1)
	override val canIntersect = shape.canIntersect

	// Refine this primitive into a collection of parts (pbrt 4.1.1)
	override def refine: Traversable[Primitive] = (shape refine) map (new GeometricPrimitive(_, material, areaLight))

	// Compute intersection between a ray and this primitive (pbrt 4.1.1)
	def intersect(ray: Ray): Option[Intersection] = shape intersect ray map { case (dg, t) => new Intersection(dg, t, this) }

	override def toString = "GeometricPrimitive(shape=%s, material=%s)" format (shape, material)
}
