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

// Primitive (pbrt 4.1)
sealed abstract class Primitive extends HasBoundingBox {
	// Compute intersection between a ray and this primitive (pbrt 4.1)
	def intersect(ray: Ray): Option[Intersection]
}

// Geometric primitive: a primitive with a shape, material and optionally an area light source
final class GeometricPrimitive private (shape: Shape, material: Material, val areaLightSource: Option[AreaLightSource]) extends Primitive {
	// Create a geometric primitive with a shape and material, no area light source
	def this(shape: Shape, material: Material) = this(shape, material, None)

	// Create a geometric primitive with an area light source and material
	def this(areaLightSource: AreaLightSource, material: Material) = this(areaLightSource.shape, material, Some(areaLightSource))

	// Bounding box that contains the primitive
	def boundingBox: BoundingBox = shape.boundingBox

	// Bounding box when primitive is transformed
	override def boundingBox(transform: Transform): BoundingBox = shape.boundingBox(transform)

	// Compute intersection between a ray and this primitive
	def intersect(ray: Ray): Option[Intersection] = shape intersect ray map { case (dg, distance) => new Intersection(dg, distance, this) }

	override def toString = "GeometricPrimitive(shape=%s, material=%s, areaLightSource=%s)" format (shape, material, areaLightSource)
}

// Transformed primitive: wraps a primitive with a transform
final class TransformedPrimitive (primitive: Primitive, transform: Transform) extends Primitive {
	private val inverse: Transform = transform.inverse

	// Bounding box that contains the primitive
	val boundingBox: BoundingBox = primitive.boundingBox(transform)

	// Bounding box when primitive is transformed
	override def boundingBox(tr: Transform): BoundingBox = primitive.boundingBox(transform * tr)

	// Compute intersection between a ray and this primitive
	def intersect(ray: Ray): Option[Intersection] = primitive intersect (inverse * ray) map { case intersection => transform * intersection }

	override def toString = "TransformedPrimitive(primitive=%s, transform=%s)" format (primitive, transform)
}

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
