/*
 * ScalaRay - Ray tracer based on pbrt (see http://pbrt.org) written in Scala
 * Copyright (C) 2009, 2010, 2011  Jesper de Jong
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

import org.jesperdj.scalaray.shape.BoundingBox
import org.jesperdj.scalaray.vecmath._

// Transformed primitive: wraps a primitive with a transform (pbrt 4.1.2)
final class TransformedPrimitive (primitive: Primitive, transform: Transform) extends Primitive {
  private val inverse: Transform = transform.inverse

  // Bounding box that contains the primitive
  val boundingBox: BoundingBox = primitive.boundingBox(transform)

  // Bounding box when primitive is transformed
  override def boundingBox(tr: Transform): BoundingBox = primitive.boundingBox(tr * transform)

  // Compute closest intersection between a ray and this primitive, returns intersection and and distance of intersection along ray
  def intersect(ray: Ray): Option[(Intersection, Double)] = primitive.intersect(inverse * ray) map {
    case (its, distance) => (transform * its, distance)
  }

  // Check if a ray intersects this primitive
  override def checkIntersect(ray: Ray): Boolean = primitive.checkIntersect(inverse * ray)

  override def toString = "TransformedPrimitive(primitive=%s, transform=%s)" format (primitive, transform)
}
