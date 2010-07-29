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
package org.jesperdj.scalaray

import org.jesperdj.scalaray.util._

package object vecmath {
	// Implicit conversion for scaling vectors by multiplying a numeric type with a vector
	implicit def implicitScaleVector[@specialized(Int, Float) N <% Float](f: N) = new MultipliableSame[Vector] {
		def *(v: Vector) = v * f
	}

	// Implicit conversion for scaling normals by multiplying a numeric type with a normal
	implicit def implicitScaleNormal[@specialized(Int, Float) N <% Float](f: N) = new MultipliableSame[Normal] {
		def *(n: Normal) = n * f
	}

	// Implicit conversion for multiplying a point by a weight
	implicit def implicitScalePoint[@specialized(Int, Float) N <% Float](f: N) = new MultipliableSame[Point] {
		def *(p: Point) = p * f
	}

	// Implicit conversion to enable Vector to be used in interpolate()
	implicit def vectorToInterpolatable(v1: Vector) = new Interpolatable[Vector] {
		def *(t: Float): Vector = v1 * t
		def +(v2: Vector): Vector = v1 + v2
	}
}
