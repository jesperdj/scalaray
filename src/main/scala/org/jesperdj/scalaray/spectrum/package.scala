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
package org.jesperdj.scalaray

package object spectrum {
	// Implicit conversion for scaling spectra by multiplying a numeric type with a spectrum
	implicit def implicitScaleSpectrum[@specialized(Int, Float) N <% Float](f: N) = new ImplicitScaleSpectrum(f)

	final class ImplicitScaleSpectrum[@specialized(Int, Float) N <% Float](f: N) {
		def *(s: Spectrum): Spectrum = s * f
	}
}
