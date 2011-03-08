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
package org.jesperdj.scalaray.vecmath

// Ray (pbrt 2.5)
sealed class Ray (val origin: Point, val direction: Vector, val minDistance: Double = 0.0, val maxDistance: Double = Double.PositiveInfinity) {
  // Check if distance t is in the valid range of this ray
  def isInRange(t: Double) = t >= minDistance && t <= maxDistance

  // Get a point along the ray
  def point(t: Double) = origin + direction * t

  override def toString = "Ray(origin=%s, direction=%s, minDistance=%g, maxDistance=%g)" format (origin, direction, minDistance, maxDistance)
}

object Ray {
  // Create a ray
  def apply(origin: Point, direction: Vector, minDistance: Double = 0.0, maxDistance: Double = Double.PositiveInfinity) =
    new Ray(origin, direction, minDistance, maxDistance)
}

// Ray differential (pbrt 2.5.1)
final class RayDifferential (origin: Point, direction: Vector, val rxOrigin: Point, val rxDirection: Vector, val ryOrigin: Point, val ryDirection: Vector,
                             minDistance: Double = 0.0, maxDistance: Double = Double.PositiveInfinity)
  extends Ray(origin, direction, minDistance, maxDistance) {

  // Create a ray differential from a ray
  def this(ray: Ray, rxOrigin: Point, rxDirection: Vector, ryOrigin: Point, ryDirection: Vector) =
    this(ray.origin, ray.direction, rxOrigin, rxDirection, ryOrigin, ryDirection, ray.minDistance, ray.maxDistance)

  override def toString =
    "RayDifferential(origin=%s, direction=%s, rxOrigin=%s, rxDirection=%s, ryOrigin=%s, ryDirection=%s, minDistance=%g, maxDistance=%g)" format
    (origin, direction, rxOrigin, rxDirection, ryOrigin, ryDirection, minDistance, maxDistance)
}

object RayDifferential {
  // Create a ray differential
  def apply(origin: Point, direction: Vector, rxOrigin: Point, rxDirection: Vector, ryOrigin: Point, ryDirection: Vector,
        minDistance: Double = 0.0, maxDistance: Double = Double.PositiveInfinity) =
          new RayDifferential(origin, direction, rxOrigin, rxDirection, ryOrigin, ryDirection, minDistance, maxDistance)

  // Create a ray differential from a ray
  def apply(ray: Ray, rxOrigin: Point, rxDirection: Vector, ryOrigin: Point, ryDirection: Vector) =
    new RayDifferential(ray, rxOrigin, rxDirection, ryOrigin, ryDirection)
}
