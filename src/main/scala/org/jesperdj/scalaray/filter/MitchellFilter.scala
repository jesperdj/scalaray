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
package org.jesperdj.scalaray.filter

// Mitchell filter (pbrt 7.7.1)
final class MitchellFilter (val extentX: Double = 2.0, val extentY: Double = 2.0, b: Double = 1.0 / 3.0, c: Double = 1.0 / 3.0) extends Filter {
  private val (p10, p11, p12, p13) = (1.0 - b / 3.0, 0.0, -3.0 + 2.0 * b + c, 2.0 - 1.5 * b - c)
  private val (p20, p21, p22, p23) = (4.0 / 3.0 * b + 4.0 * c, -2.0 * b - 8.0 * c, b + 5.0 * c, -b / 6.0 - c)

  private def mitchell(v: Double) = {
    val x = 2.0 * v.abs
    if (x <= 1.0) p10 + p11 * x + p12 * x * x + p13 * x * x * x else p20 + p21 * x + p22 * x * x + p23 * x * x * x
  }

  def apply(x: Double, y: Double) = mitchell(x / extentX) * mitchell(y / extentY)

  override def toString = "MitchellFilter(extentX=%g, extentY=%g, b=%g, c=%g)" format (extentX, extentY, b, c)
}
