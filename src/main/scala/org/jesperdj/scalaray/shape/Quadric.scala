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

import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// Quadric, superclass for quadric shapes
abstract class Quadric extends Shape {
  // Compute closest intersection between a ray and this shape, returns differential geometry and distance of intersection along ray
  def intersect(ray: Ray): Option[(DifferentialGeometry, Float)] = {
    // Get differential geometry and distance if the intersection point is in the range of the ray
    def getResult(distance: Float): Option[(DifferentialGeometry, Float)] =
      if (ray.isInRange(distance)) differentialGeometry(ray.point(distance)) map { case dg => (dg, distance) } else None

    // Compute quadratic coefficients
    val (a, b, c) = computeCoefficients(ray)

    // Solve quadratic equation
    val d = b * b - 4.0f * a * c
    if (d < 0.0f) None else {
      val q = if (b < 0.0f) -0.5f * (b - math.sqrt(d).toFloat) else -0.5f * (b + math.sqrt(d).toFloat)
      val (t0, t1) = minmax(q / a, c / q)

      // Return differential geometry and distance for closest valid intersection point
      getResult(t0) orElse getResult(t1)
    }
  }

  // Compute quadratic coefficients
  protected def computeCoefficients(ray: Ray): (Float, Float, Float)

  // Get differential geometry for an intersection point
  protected def differentialGeometry(point: Point): Option[DifferentialGeometry]
}
