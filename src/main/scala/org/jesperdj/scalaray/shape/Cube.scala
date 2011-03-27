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

import org.jesperdj.scalaray.vecmath._

// Unit cube
object Cube extends Shape {
  // Bounding box that contains the shape
  val boundingBox: BoundingBox = BoundingBox(Point.Origin, Point(1.0, 1.0, 1.0))

  // Surface normals of the sides (left, right, bottom, top, front, back)
  private val normals = Array(-Normal.XAxis, Normal.XAxis, -Normal.YAxis, Normal.YAxis, -Normal.ZAxis, Normal.ZAxis)

  // Partial derivatives of the surface position of the sides (left / right, bottom / top, front / back)
  private val dpdus = Array(Vector.YAxis, Vector.ZAxis, Vector.XAxis)
  private val dpdvs = Array(Vector.ZAxis, Vector.XAxis, Vector.YAxis)

  // Compute closest intersection between a ray and this shape, returns differential geometry and distance of intersection along ray
  def intersect(ray: Ray): Option[(DifferentialGeometry, Double)] = {
    // Find valid intersection with one of the sides
    def intersect(index: Int, pd: Double): Option[(Double, Double, Double)] = {
      val rd = ray.direction(index)
      if (rd == 0.0) None else {
        val distance = (pd - ray.origin(index)) / rd
        if (!ray.isInRange(distance)) None else {
          val ia = (index + 1) % 3
          val a = ray.origin(ia) + distance * ray.direction(ia)
          if (a < 0.0 || a > 1.0) return None

          val ib = (index + 2) % 3
          val b = ray.origin(ib) + distance * ray.direction(ib)
          if (b < 0.0 || b > 1.0) return None

          Some(distance, a, b)
        }
      }
    }

    // Find valid intersections with all six faces (left, right, bottom, top, front, back)
    val ts = Array(intersect(0, 0.0), intersect(0, 1.0), intersect(1, 0.0), intersect(1, 1.0), intersect(2, 0.0), intersect(2, 1.0))

    // Find the closest intersection
    val result = ts.zipWithIndex.foldLeft(None: Option[(Double, Double, Double, Int)]) {
      case (res, (Some((distance, a, b)), index)) => if (!res.isDefined || distance < res.get._1) Some(distance, a, b, index) else res
      case (res, _) => res
    }

    result match {
      case Some((distance, a, b, index)) =>
        Some((new DifferentialGeometry {
          // Intersection point
          lazy val point: Point = ray(distance)

          // Surface normal
          val normal: Normal = normals(index)

          // Surface parameter coordinates
          val u: Double = a
          val v: Double = b

          // Partial derivatives of the surface position
          val dpdu: Vector = dpdus(index / 2)
          val dpdv: Vector = dpdvs(index / 2)

          // Partial derivatives of the surface normal
          val dndu: Normal = Normal.Zero
          val dndv: Normal = Normal.Zero

          // Shape which is intersected
          val shape: Shape = Cube.this
        }, distance))

      case None => None
    }
  }

  // Surface area
  val surfaceArea: Double = 6.0

  // Sample a point on the surface using the random variables u1, u2
  // Returns a point on the surface, the surface normal at that point and the probability density for this sample
  def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = {
    // TODO: Implement this
    throw new UnsupportedOperationException("Not yet implemented")
  }
}
