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

// kd-tree accelerator
final class KdTreeAccelerator (primitives: Traversable[Primitive]) extends Primitive with Accelerator {
	// Bounding box that contains the primitive
	val boundingBox: BoundingBox = BoundingBox.Empty // TODO: Implement boundingBox

	// Bounding box when primitive is transformed
	override def boundingBox(transform: Transform): BoundingBox =
		throw new UnsupportedOperationException("Not yet implemented") // TODO: Implement boundingBox
	
	// Compute intersection between a ray and this primitive
	def intersect(ray: Ray): Option[Intersection] =
		throw new UnsupportedOperationException("Not yet implemented") // TODO: Implement intersect

	override def toString = "KdTreeAccelerator"
}
