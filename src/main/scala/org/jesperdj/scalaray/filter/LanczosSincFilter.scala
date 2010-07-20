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
package org.jesperdj.scalaray.filter

import org.jesperdj.scalaray.util._

// Windowed sinc filter (pbrt 7.6.1)
final class LanczosSincFilter (extentX: Double = 4.0, extentY: Double = 4.0, tau: Double = 3.0) extends Filter(extentX, extentY) {
	private def lanczosSinc(v: Double) = {
		val x = math.abs(v)
		if (x < 1e-6) 1.0 else if (x > 1.0) 0.0 else {
			val w = π * x; val wt = w * tau; (math.sin(wt) / wt) * (math.sin(w) / w)
		}
	}

	def apply(x: Double, y: Double) = lanczosSinc(x / extentX) * lanczosSinc(y / extentY)

	override def toString = "LanczosSincFilter(extentX=%g, extentY=%g, tau=%g)" format (extentX, extentY, tau)
}
