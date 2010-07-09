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

import org.jesperdj.scalaray.lightsource._
import org.jesperdj.scalaray.material._
import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.vecmath._

// Geometric primitive: a primitive with a shape, material and optionally an area light source
final class GeometricPrimitive private (shape: Shape, material: Material, val areaLightSource: Option[AreaLightSource]) extends Primitive {
	// Create a geometric primitive with a shape and material, no area light source
	def this(shape: Shape, material: Material) = this(shape, material, None)

	// Create a geometric primitive with an area light source and material
	def this(areaLightSource: AreaLightSource, material: Material) = this(areaLightSource.shape, material, Some(areaLightSource))

	// Bounding box that contains the primitive
	val boundingBox: BoundingBox = shape.boundingBox

	// Bounding box when primitive is transformed
	override def boundingBox(transform: Transform): BoundingBox = shape.boundingBox(transform)

	// Compute intersection between a ray and this primitive
	def intersect(ray: Ray): Option[Intersection] = shape intersect ray map { case (dg, distance) => new Intersection(dg, distance, this) }

	override def toString = "GeometricPrimitive(shape=%s, material=%s, areaLightSource=%s)" format (shape, material, areaLightSource)
}
