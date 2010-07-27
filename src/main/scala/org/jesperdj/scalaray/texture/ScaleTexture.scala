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
package org.jesperdj.scalaray.texture

import org.jesperdj.scalaray.shape.DifferentialGeometry
import org.jesperdj.scalaray.util._

// Scale texture (pbrt 10.3.2)
final class ScaleTexture[@specialized(Float) T1 <% Multipliable[T2, T2], @specialized(Float) T2] (tex1: Texture[T1], tex2: Texture[T2]) extends Texture[T2] {
	def apply(dg: DifferentialGeometry): T2 = tex1(dg) * tex2(dg)
}
