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
package org.jesperdj.scalaray.shape

import org.jesperdj.scalaray._
import org.jesperdj.scalaray.vecmath._

// Quadric, superclass for quadric shapes
abstract class Quadric extends Shape {
	// Compute intersection between a ray and this shape
	def intersect(ray: Ray): Option[(DifferentialGeometry, Double)] = {
		// Transform ray to object coordinates
		val r = worldToObject * ray

		// Get differential geometry and distance if the intersection point is in the range of the ray
		def getResult(t: Double): Option[(DifferentialGeometry, Double)] =
			if (r.isInRange(t)) differentialGeometry(r.point(t)) map { case dg => (dg, t) } else None

		// Compute quadratic coefficients
		val (a, b, c) = computeCoefficients(r)

		// Solve quadratic equation
		val d = b * b - 4.0 * a * c
		if (d < 0.0) None else {
			val q = if (b < 0.0) -0.5 * (b - math.sqrt(d)) else -0.5 * (b + math.sqrt(d))
			var (t0, t1) = minmax(q / a, c / q)

			// Return differential geometry and distance for closest valid intersection point
			getResult(t0) orElse getResult(t1)
		}
	}

	// Compute quadratic coefficients
	protected def computeCoefficients(r: Ray): (Double, Double, Double)

	// Get differential geometry for an intersection point
	protected def differentialGeometry(p: Point): Option[DifferentialGeometry]
}
