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
package org.jesperdj.scalaray.reflection

import org.jesperdj.scalaray.spectrum._
import org.jesperdj.scalaray.vecmath._

// TODO: Not yet implemented

// Superclass for BRDF and BTDF
sealed abstract class BxDF {
	// TODO: Description
	def apply(wo: Vector, wi: Vector): Spectrum

	// TODO: Description. Returns reflectance, wi and pdf
	def sample(wo: Vector, u1: Double, u2: Double): (Spectrum, Vector, Double)
}

// Bidirectional Reflectance Distribution Function
abstract class BRDF extends BxDF

// Bidirectional Transmittance Distribution Function
abstract class BTDF extends BxDF
