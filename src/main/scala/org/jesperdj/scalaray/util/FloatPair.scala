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
package org.jesperdj.scalaray.util

// A pair of Floats. Use this instead of Tuple2[Float, Float]; Tuple2 is not @specialized for Float in Scala 2.8, so it will do unnecessary boxing and unboxing.
final class FloatPair (val _1: Float, val _2: Float) {
	override def toString = "(" + _1 + ", " + _2 + ")"
}

object FloatPair {
	def apply(_1: Float, _2: Float): FloatPair = new FloatPair(_1, _2)
}
