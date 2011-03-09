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
package org.jesperdj.scalaray.shape

import scala.collection.immutable.Traversable

import org.jesperdj.scalaray.vecmath._

// Axis-aligned bounding box (pbrt 2.6)
sealed class BoundingBox (points: Traversable[Point]) {
  // Create a bounding box from points (using varargs)
  def this(points: Point*) = this(Traversable(points: _*))

  // Minimum and maximum corners
  val (min, max) = {
    var (minX, minY, minZ) = (Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
    var (maxX, maxY, maxZ) = (Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity)

    for (p <- points) {
      if (p.x < minX) minX = p.x; if (p.y < minY) minY = p.y; if (p.z < minZ) minZ = p.z
      if (p.x > maxX) maxX = p.x; if (p.y > maxY) maxY = p.y; if (p.z > maxZ) maxZ = p.z
    }

    (Point(minX, minY, minZ), Point(maxX, maxY, maxZ))
  }

  // Centroid of the bounding box
  val centroid = min + ((max - min) / 2.0)

  // Sphere that contains this bounding box; returns center point and radius
  def boundingSphere: (Point, Double) = (centroid, if (inside(centroid)) centroid.distance(max) else 0.0)

  // The eight corners of the bounding box
  def corners: Traversable[Point] = Traversable(
    min, Point(min.x, min.y, max.z), Point(min.x, max.y, min.z), Point(min.x, max.y, max.z),
    Point(max.x, min.y, min.z), Point(max.x, min.y, max.z), Point(max.x, max.y, min.z), max)

  // Union between this bounding box and a point
  def union(p: Point) = if (inside(p)) this else new BoundingBox(min, max, p)

  // Union between this and another bounding box
  def union(bb: BoundingBox) = new BoundingBox(min, max, bb.min, bb.max)

  // Check if a point is inside this bounding box
  def inside(p: Point) =
    (p.x >= min.x) && (p.x <= max.x) &&
    (p.y >= min.y) && (p.y <= max.y) &&
    (p.z >= min.z) && (p.z <= max.z)

  // Check if another bounding box overlaps with this one
  def overlaps(bb: BoundingBox) =
    (max.x >= bb.min.x) && (min.x <= bb.max.x) &&
    (max.y >= bb.min.y) && (min.y <= bb.max.y) &&
    (max.z >= bb.min.z) && (min.z <= bb.max.z)

  // Compute intersection between a ray and this bounding box; returns an Option with the range of the ray that's inside the box (pbrt 4.2.1)
  def intersect(ray: Ray): Option[(Double, Double)] = {
    var minT = ray.minDistance
    var maxT = ray.maxDistance

    var t0 = (min.x - ray.origin.x) / ray.direction.x
    var t1 = (max.x - ray.origin.x) / ray.direction.x
    if (t0 > t1) { val t = t0; t0 = t1; t1 = t }

    minT = math.max(minT, t0)
    maxT = math.min(maxT, t1)
    if (minT > maxT) return None

    t0 = (min.y - ray.origin.y) / ray.direction.y
    t1 = (max.y - ray.origin.y) / ray.direction.y
    if (t0 > t1) { val t = t0; t0 = t1; t1 = t }

    minT = math.max(minT, t0)
    maxT = math.min(maxT, t1)
    if (minT > maxT) return None

    t0 = (min.z - ray.origin.z) / ray.direction.z
    t1 = (max.z - ray.origin.z) / ray.direction.z
    if (t0 > t1) { val t = t0; t0 = t1; t1 = t }

    minT = math.max(minT, t0)
    maxT = math.min(maxT, t1)
    if (minT > maxT) return None

    Some((minT, maxT))
  }

  // Surface area
  val surfaceArea = { val d = max - min; 2.0 * (d.x * d.y + d.x * d.z + d.y * d.z) }

  // Volume
  val volume = { val d = max - min; d.x * d.y * d.z }

  override def toString = "BoundingBox(min=%s, max=%s)" format (min, max)
}

object BoundingBox {
  // BoundingBox constants
  val Empty: BoundingBox = new BoundingBox {
//    override val min = Point.PositiveInfinity  NOTE: Uncommenting this causes a NullPointerException because of an initialization order problem
//    override val max = Point.NegativeInfinity
    override val centroid = Point.Origin
    override val boundingSphere: (Point, Double) = (Point.Origin, 0.0)
    override val corners: Traversable[Point] = Traversable.empty
    override def union(p: Point) = new BoundingBox(p)
    override def union(bb: BoundingBox) = bb
    override def inside(p: Point) = false
    override def overlaps(bb: BoundingBox) = false
    override def intersect(ray: Ray): Option[(Double, Double)] = None
    override val surfaceArea = 0.0
    override val volume = 0.0

    override def toString = "BoundingBox.Empty"
  }

  // Create a bounding box
  def apply(points: Traversable[Point]) = new BoundingBox(points)

  // Create a bounding box from points (using varargs)
  def apply(points: Point*) = new BoundingBox(points: _*)

  // Extractor method
  def unapply(bb: BoundingBox) = Some(bb.min, bb.max)
}
