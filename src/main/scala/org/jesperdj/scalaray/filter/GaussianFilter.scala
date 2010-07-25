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

// Gaussian filter (pbrt 7.6.1)
final class GaussianFilter (extentX: Float = 2.0f, extentY: Float = 2.0f, alpha: Float = 2.0f) extends Filter(extentX, extentY) {
	private val expX = math.exp(-alpha * extentX * extentX).toFloat
	private val expY = math.exp(-alpha * extentY * extentY).toFloat

	private def gaussian(d: Float, exp: Float) = math.max(0.0f, math.exp(-alpha * d * d).toFloat - exp)

	def apply(x: Float, y: Float) = gaussian(x, expX) * gaussian(y, expY)

	override def toString = "GaussianFilter(extentX=%g, extentY=%g, alpha=%g)" format (extentX, extentY, alpha)
}
