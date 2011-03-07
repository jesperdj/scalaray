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

import org.jesperdj.scalaray.util._

// Windowed sinc filter (pbrt 7.6.1)
final class LanczosSincFilter (extentX: Float = 4.0f, extentY: Float = 4.0f, tau: Float = 3.0f) extends Filter(extentX, extentY) {
  private def lanczosSinc(v: Float) = {
    val x = v.abs
    if (x < 1e-6f) 1.0f else if (x > 1.0f) 0.0f else {
      val w = π * x; val wt = w * tau; (math.sin(wt).toFloat / wt) * (math.sin(w).toFloat / w)
    }
  }

  def apply(x: Float, y: Float) = lanczosSinc(x / extentX) * lanczosSinc(y / extentY)

  override def toString = "LanczosSincFilter(extentX=%g, extentY=%g, tau=%g)" format (extentX, extentY, tau)
}
