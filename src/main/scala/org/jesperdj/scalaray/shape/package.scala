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

import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

package object shape {
  // Implicit conversion for transforming a bounding box (pbrt 2.8.5)
  implicit def implicitTransformBoundingBox(t: Transform) = new MultipliableSame[BoundingBox] {
    def *(bb: BoundingBox) = new BoundingBox(bb.corners map (t * _ ))
  }

  // Implicit conversion for transforming a DifferentialGeometry
  implicit def implicitTransformDifferentialGeometry(t: Transform) = new MultipliableSame[DifferentialGeometry] {
    def *(dg: DifferentialGeometry) = new DifferentialGeometry {
      // NOTE: This class depends on the fact that the fields of DifferentialGeometry have value semantics (the fields must be vals, not defs)
      // If the fields in DifferentialGeometry would be defs, they should be defs here as well instead of (lazy) vals

      // Point on the surface
      lazy val point: Point = t * dg.point

      // Surface normal at the point
      lazy val normal: Normal = (t * dg.normal).normalize

      // Surface parameter coordinates
      val u: Float = dg.u
      val v: Float = dg.v

      // Partial derivatives of the surface position
      lazy val dpdu: Vector = t * dg.dpdu
      lazy val dpdv: Vector = t * dg.dpdv

      // Partial derivatives of the surface normal
      lazy val dndu: Normal = t * dg.dndu
      lazy val dndv: Normal = t * dg.dndv

      // Shape which defines the surface
      val shape = dg.shape
    }
  }
}
