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

import org.jesperdj.scalaray.vecmath._

// Shape, describes geometry of a surface
abstract class Shape extends HasBoundingBox {
	// Compute intersection between a ray and this shape, returns differential geometry and distance of intersection along ray
	def intersect(ray: Ray): Option[(DifferentialGeometry, Double)]

	// Surface area
	def surfaceArea: Double

	// TODO: surfaceArea klopt alleen als de shape niet wordt getransformed met een scaling transform!
	// Daardoor werken area lights, die hiervan gebruik maken, niet op de juiste manier als je de shape met een scaling transform transformeert.

	// Sample a point on the surface using the random variables u1, u2
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(u1: Double, u2: Double): (Point, Normal, Double)

	// Sample a point on the surface with respect to a point from which the shape is visible using the random variables u1, u2
	// Returns a point on the surface, the surface normal at that point and the value of the probability distribution function for this sample
	def sampleSurface(point: Point, u1: Double, u2: Double): (Point, Normal, Double) = sampleSurface(u1, u2)
}
