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

// Differential geometry, describes the geometry at a point on a surface
abstract class DifferentialGeometry {
	// Point on the surface
	val point: Point

	// Surface normal at the point
	val normal: Normal

	// Surface parameter coordinates
	val uv: (Double, Double)

	// Shape which defines the surface
	val shape: Shape

	override def toString = "DifferentialGeometry(point=%s, normal=%s, uv=%s, shape=%s)" format (point, normal, uv, shape)
}
