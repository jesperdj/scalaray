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

// Gaussian filter (pbrt 7.6.1)
final class GaussianFilter (extentX: Double = 2.0, extentY: Double = 2.0, alpha: Double = 2.0) extends Filter(extentX, extentY) {
  private val expX = math.exp(-alpha * extentX * extentX)
  private val expY = math.exp(-alpha * extentY * extentY)

  private def gaussian(d: Double, exp: Double) = math.max(0.0, math.exp(-alpha * d * d) - exp)

  def apply(x: Double, y: Double) = gaussian(x, expX) * gaussian(y, expY)

  override def toString = "GaussianFilter(extentX=%g, extentY=%g, alpha=%g)" format (extentX, extentY, alpha)
}
