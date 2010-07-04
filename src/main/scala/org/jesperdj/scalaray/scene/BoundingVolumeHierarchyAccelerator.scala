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

import org.jesperdj.scalaray._
import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.vecmath._

// Bounding volume hierarchy accelerator (pbrt 4.4)
final class BoundingVolumeHierarchyAccelerator
	(primitives: Traversable[Primitive],
	 split: (Traversable[Primitive]) => (Traversable[Primitive], Traversable[Primitive]) = BoundingVolumeHierarchyAccelerator.splitSurfaceAreaHeuristic,
	 maxPrimitivesPerNode: Int = 2) extends Primitive {

	require(maxPrimitivesPerNode >= 2, "maxPrimitivesPerNode must be >= 2")

	private val root = {
		// Recursive function to build the tree
		def build(ps: Traversable[Primitive]): Primitive = {
			if (ps.size == 1) ps.head else if (ps.size <= maxPrimitivesPerNode) new AggregatePrimitive(ps) else {
				val (left, right) = split(ps)
				new AggregatePrimitive(build(left), build(right))
			}
		}

		// Fully refine all primitives and build the tree
		build(primitives flatMap (_.fullyRefine))
	}

	// Bounding box in world coordinates
	val worldBound = root.worldBound

	// Compute intersection between a ray and this primitive
	def intersect(ray: Ray): Option[Intersection] = root intersect ray

	override def toString = "BoundingVolumeHierarchyAccelerator"
}

object BoundingVolumeHierarchyAccelerator {
	def splitMiddle(ps: Traversable[Primitive]): (Traversable[Primitive], Traversable[Primitive]) = {
		// Compute bounding box of centroids and extents of that bounding box
		val cb = ps.foldLeft(BoundingBox.Empty) { (bb, p) => bb union p.worldBound.centroid }
		val (ex, ey, ez) = (cb.max.x - cb.min.x, cb.max.y - cb.min.y, cb.max.z - cb.min.z)

		// Predicates for partitioning
		def predX(p: Primitive) = p.worldBound.centroid.x < cb.centroid.x
		def predY(p: Primitive) = p.worldBound.centroid.y < cb.centroid.y
		def predZ(p: Primitive) = p.worldBound.centroid.z < cb.centroid.z

		// Select predicate for the largest extent axis
		val pred: (Primitive) => Boolean = if (ex > ey && ex > ez) predX else if (ey > ex && ey > ez) predY else predZ

		ps partition pred
	}

	def splitEqualCounts(ps: Traversable[Primitive]): (Traversable[Primitive], Traversable[Primitive]) = (Traversable(ps.head), ps.tail) // TODO

	def splitSurfaceAreaHeuristic(ps: Traversable[Primitive]): (Traversable[Primitive], Traversable[Primitive]) = (Traversable(ps.head), ps.tail) // TODO
}
