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

import scala.collection.immutable.Traversable

import org.jesperdj.scalaray.lightsource.LightSource
import org.jesperdj.scalaray.shape.BoundingBox
import org.jesperdj.scalaray.vecmath._

// Scene (pbrt 1.3.2)
final class Scene (primitive: Primitive, val lightSources: Traversable[LightSource]) {
  // Bounding box of the whole scene
  def boundingBox: BoundingBox = primitive.boundingBox

  // Bounding sphere of the whole scene
  def boundingSphere: (Point, Float) = boundingBox.boundingSphere

  // Compute closest intersection between a ray and primitives in the scene
  def intersect(ray: Ray): Option[Intersection] = primitive intersect ray map { case (its, _) => its }

  // Check if a ray intersects a primitive in the scene
  def checkIntersect(ray: Ray): Boolean = primitive checkIntersect ray

  override def toString = "Scene(primitive=%s, lightSources=%s)" format (primitive, lightSources)
}
