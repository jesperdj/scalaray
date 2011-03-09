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
package org.jesperdj.scalaray

import org.jesperdj.scalaray.common._

package object vecmath {
  // Implicit conversion for scaling vectors by multiplying a numeric type with a vector
  implicit def implicitScaleVector[@specialized(Int, Double) T <% Double](f: T) = new MultipliableSame[Vector] {
    @inline def *(v: Vector) = v * f
  }

  // Implicit conversion for scaling normals by multiplying a numeric type with a normal
  implicit def implicitScaleNormal[@specialized(Int, Double) T <% Double](f: T) = new MultipliableSame[Normal] {
    @inline def *(n: Normal) = n * f
  }

  // Implicit conversion for multiplying a point by a weight
  implicit def implicitScalePoint[@specialized(Int, Double) T <% Double](f: T) = new MultipliableSame[Point] {
    @inline def *(p: Point) = p * f
  }

  // Implicit conversion for scaling quaternions by multiplying a numeric type with a quaternion
  implicit def implicitScaleQuaternion[@specialized(Int, Double) T <% Double](f: T) = new MultipliableSame[Quaternion] {
    @inline def *(q: Quaternion) = q * f
  }

  // Implicit conversion to enable Vector to be used in interpolate()
  implicit def vectorToInterpolatable(v: Vector) = new Interpolatable[Vector] {
    @inline def *(f: Double): Vector = v * f
    @inline def +(w: Vector): Vector = v + w
  }
}
