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
package org.jesperdj.scalaray.accelerator

import scala.collection.immutable.IndexedSeq

import org.jesperdj.scalaray.scene.{ CompositePrimitive, Intersection, Primitive }
import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.vecmath._

// Bounding volume hierarchy accelerator (pbrt 4.4)
final class BoundingVolumeHierarchyAccelerator (primitives: IndexedSeq[Primitive],
                                                split: IndexedSeq[Primitive] => (IndexedSeq[Primitive], IndexedSeq[Primitive]) = BoundingVolumeHierarchyAccelerator.splitSurfaceAreaHeuristic,
                                                maxPrimitivesPerNode: Int = 2) extends Primitive with Accelerator {
  require(maxPrimitivesPerNode >= 2, "maxPrimitivesPerNode must be >= 2")

  private val root: Primitive = {
    // Recursive method to build the tree
    def build(ps: IndexedSeq[Primitive]): Primitive = {
      if (ps.size == 1) ps.head else if (ps.size <= maxPrimitivesPerNode) new CompositePrimitive(ps) else {
        val (left, right) = split(ps)
        new CompositePrimitive(build(left), build(right))
      }
    }

    build(primitives)
  }

  // Bounding box that contains the primitive
  val boundingBox: BoundingBox = root.boundingBox

  // Bounding box when primitive is transformed
  override def boundingBox(transform: Transform): BoundingBox = root.boundingBox(transform)

  // Compute closest intersection between a ray and this primitive, returns intersection and and distance of intersection along ray
  def intersect(ray: Ray): Option[(Intersection, Double)] = root.intersect(ray)

  // Check if a ray intersects this primitive
  override def checkIntersect(ray: Ray): Boolean = root.checkIntersect(ray)

  override def toString = "BoundingVolumeHierarchyAccelerator(root=%s)" format (root)
}

object BoundingVolumeHierarchyAccelerator {
  // Split primitives by comparing their centroid to the centroid of the collection; partition along the axis with the largest extent (pbrt 4.4.1)
  def splitMiddle(ps: IndexedSeq[Primitive]): (IndexedSeq[Primitive], IndexedSeq[Primitive]) = {
    // Compute bounding box of centroids and extents of that bounding box
    val cb = ps.foldLeft(BoundingBox.Empty) { (bb, p) => bb union p.boundingBox.centroid }
    val (ex, ey, ez) = (cb.max.x - cb.min.x, cb.max.y - cb.min.y, cb.max.z - cb.min.z)

    // Predicates for partitioning
    def predX(p: Primitive) = p.boundingBox.centroid.x < cb.centroid.x
    def predY(p: Primitive) = p.boundingBox.centroid.y < cb.centroid.y
    def predZ(p: Primitive) = p.boundingBox.centroid.z < cb.centroid.z

    // Select predicate for the largest extent axis
    val pred: (Primitive) => Boolean = if (ex > ey && ex > ez) predX else if (ey > ex && ey > ez) predY else predZ

    val (left, right) = ps partition pred
    if (left.isEmpty) (IndexedSeq(right.head), right.tail) else (left, right)
  }

  // Sort primitives by their centroids along the axis with the largest extent, then split the collection so that both parts
  // have an approximately equal number of primitives (pbrt 4.4.1)
  def splitEqualCounts(ps: IndexedSeq[Primitive]): (IndexedSeq[Primitive], IndexedSeq[Primitive]) = {
    // Compute bounding box of centroids and extents of that bounding box
    val cb = ps.foldLeft(BoundingBox.Empty) { (bb, p) => bb union p.boundingBox.centroid }
    val (ex, ey, ez) = (cb.max.x - cb.min.x, cb.max.y - cb.min.y, cb.max.z - cb.min.z)

    // Comparison functions for sorting
    def compareX(p1: Primitive, p2: Primitive) = p1.boundingBox.centroid.x < p2.boundingBox.centroid.x
    def compareY(p1: Primitive, p2: Primitive) = p1.boundingBox.centroid.y < p2.boundingBox.centroid.y
    def compareZ(p1: Primitive, p2: Primitive) = p1.boundingBox.centroid.z < p2.boundingBox.centroid.z

    // Select the comparison function for the largest extent axis
    val compare: (Primitive, Primitive) => Boolean = if (ex > ey && ex > ez) compareX else if (ey > ex && ey > ez) compareY else compareZ

    val sorted = ps sortWith compare
    sorted.splitAt(sorted.size / 2)
  }

  // Split primitives according to the surface area heuristic (pbrt 4.4.2)
  def splitSurfaceAreaHeuristic(ps: IndexedSeq[Primitive]): (IndexedSeq[Primitive], IndexedSeq[Primitive]) =
    throw new UnsupportedOperationException("Not yet implemented") // TODO: Implement splitSurfaceAreaHeuristic
}
