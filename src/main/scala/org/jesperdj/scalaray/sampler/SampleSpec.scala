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
package org.jesperdj.scalaray.sampler

import org.jesperdj.scalaray._

// Sample specification, used to configure a sampler to produce 1D and 2D sample patterns for each sample
sealed abstract class SampleSpec extends UniqueId

// Sample specification for a 1D sample pattern
final class SampleSpec1D (val count: Int) extends SampleSpec {
	override def toString = "SampleSpec1D(id=%d, count=%d)" format (id, count)
}

// Sample specification for a 2D sample pattern
final class SampleSpec2D (val countX: Int, val countY: Int) extends SampleSpec {
	override def toString = "SampleSpec2D(id=%d, countX=%d, countY=%d)" format (id, countX, countY)
}