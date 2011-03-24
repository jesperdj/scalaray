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
final class Cube extends Shape {
  // Bounding box that contains the shape
  val boundingBox: BoundingBox = BoundingBox(Point.Origin, Point(1.0, 1.0, 1.0))

  // Compute closest intersection between a ray and this shape, returns differential geometry and distance of intersection along ray
  def intersect(ray: Ray): Option[(DifferentialGeometry, Double)] = {
    boundingBox.intersect(ray) map {
      case (distance, _) => (new DifferentialGeometry {
        // Intersection point
        lazy val point: Point = ray(distance)

        // Surface normal
        lazy val normal: Normal = Normal.Zero // TODO: Depends on which face of the cube was intersected

        // Surface parameter coordinates
        val u: Double = 0.0 // TODO
        val v: Double = 0.0 // TODO

        // Partial derivatives of the surface position
        val dpdu: Vector = Vector.Zero // TODO
        val dpdv: Vector = Vector.Zero // TODO

        // Partial derivatives of the surface normal
        val dndu: Normal = Normal.Zero
        val dndv: Normal = Normal.Zero

        // Shape which is intersected
        val shape: Shape = Cube.this
      }, distance)
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
