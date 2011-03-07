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
package org.jesperdj.scalaray.texture

import org.jesperdj.scalaray.shape.DifferentialGeometry
import org.jesperdj.scalaray.util._

// Mix texture (pbrt 10.3.3)
final class MixTexture[@specialized(Float) T <% Interpolatable[T]] (tex1: Texture[T], tex2: Texture[T], amount: Texture[Float]) extends Texture[T] {
  def apply(dg: DifferentialGeometry): T = interpolate(amount(dg), tex1(dg), tex2(dg))
}
