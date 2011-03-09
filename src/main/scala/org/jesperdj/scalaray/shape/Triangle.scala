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

import org.jesperdj.scalaray.sampler.SampleTransforms
import org.jesperdj.scalaray.vecmath._

// Vertex
final class Vertex (val point: Point, val normal: Normal, val u: Double, val v: Double) {
  override def toString = "Vertex(point=%s, normal=%s, u=%g, v=%g)" format (point, normal, u, v)
}

// Triangle (pbrt 3.6)
final class Triangle (v0: Vertex, v1: Vertex, v2: Vertex) extends Shape {
  // Edge vectors
  private val e1: Vector = v1.point - v0.point
  private val e2: Vector = v2.point - v0.point

  // Surface normal
  private val normal: Normal = Normal(e1 ** e2).normalize

  // Bounding box that contains the object
  val boundingBox: BoundingBox = BoundingBox(v0.point, v1.point, v2.point)

  // Bounding box when object is transformed
  override def boundingBox(transform: Transform): BoundingBox = BoundingBox(transform * v0.point, transform * v1.point, transform * v2.point)

  // Compute closest intersection between a ray and this shape, returns differential geometry and distance of intersection along ray
  def intersect(ray: Ray): Option[(DifferentialGeometry, Double)] = {
    val s1 = ray.direction ** e2
    val div = s1 * e1
    if (div == 0.0) return None

    // Compute first barycentric coordinate
    val d = ray.origin - v0.point
    val b1 = (d * s1) / div
    if (b1 < 0.0 || b1 > 1.0) return None

    // Compute second barycentric coordinate
    val s2 = d ** e1
    val b2 = (ray.direction * s2) / div
    if (b2 < 0.0 || b1 + b2 > 1.0) return None

    // Compute distance to intersection point
    val distance = (e2 * s2) / div
    if (!ray.isInRange(distance)) return None

    // Initialize differential geometry
    Some((new DifferentialGeometry {
      // Intersection point
      lazy val point: Point = ray(distance)

      // Surface normal
      val normal: Normal = Triangle.this.normal

      // Surface parameter coordinates; interpolate from vertices
      lazy val (u, v) = {
        val b0 = 1.0 - b1 - b2
        (b0 * v0.u + b1 * v1.u + b2 * v2.u, b0 * v0.v + b1 * v1.v + b2 * v2.v)
      }

      // Partial derivatives of the surface position
      lazy val (dpdu, dpdv) = {
        val du1 = v0.u - v2.u
        val du2 = v1.u - v2.u
        val dv1 = v0.v - v2.v
        val dv2 = v1.v - v2.v

        val dp1 = v0.point - v2.point
        val dp2 = v1.point - v2.point

        val det = du1 * dv2 - dv1 * du2

        if (det != 0.0) ((dp1 * dv2 - dp2 * dv1) / det, (dp1 * -du2 + dp2 * du1) / det)
        else Vector.coordinateSystem((e2 ** e1).normalize)
      }

      // Partial derivatives of the surface normal
      val dndu: Normal = Normal.Zero
      val dndv: Normal = Normal.Zero

      // Shape which is intersected
      val shape: Shape = Triangle.this
    }, distance))
  }

  // Surface area
  val surfaceArea: Double = (e1 ** e2).length / 2.0

  // Sample a point on the surface using the random variables u1, u2 (pbrt 14.6.3)
  // Returns a point on the surface, the surface normal at that point and the probability density for this sample
  def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double) = {
    val (b1, b2) = SampleTransforms.uniformSampleTriangle(u1, u2)
    (v0.point + (e1 * b1 + e2 * b2), normal, 1.0 / surfaceArea)
    // TODO: Is the normal being computed correctly here? What about interpolating vertex normals, or is that only for shading geometry?
  }

  override def toString = "Triangle(%s, %s, %s)" format (v0, v1, v2)
}
