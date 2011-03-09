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

// Differential geometry, describes the geometry at a point on a surface (pbrt 2.10)
trait DifferentialGeometry {
  // Point on the surface
  val point: Point

  // Surface normal at the point
  val normal: Normal

  // Surface parameter coordinates
  val u: Double
  val v: Double

  // Partial derivatives of the surface position
  val dpdu: Vector
  val dpdv: Vector

  // Partial derivatives of the surface normal
  val dndu: Normal
  val dndv: Normal

  // Shape which defines the surface
  val shape: Shape

  override def toString = "DifferentialGeometry(point=%s, normal=%s, u=%g, v=%g, shape=%s)" format (point, normal, u, v, shape)
}
