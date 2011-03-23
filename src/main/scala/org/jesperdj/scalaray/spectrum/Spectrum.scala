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
package org.jesperdj.scalaray.spectrum

// Spectrum
sealed class Spectrum (private val red: Double, private val green: Double, private val blue: Double) {
  // Add two spectra
  def +(s: Spectrum): Spectrum = new Spectrum(red + s.red, green + s.green, blue + s.blue)

  // Subtract two spectra
  def -(s: Spectrum): Spectrum = new Spectrum(red - s.red, green - s.green, blue - s.blue)

  // Multiply two spectra
  def *(s: Spectrum): Spectrum = new Spectrum(red * s.red, green * s.green, blue * s.blue)

  // Divide two spectra
  def /(s: Spectrum): Spectrum = new Spectrum(red / s.red, green / s.green, blue / s.blue)

  // Scale this spectrum
  def *(f: Double): Spectrum = new Spectrum(red * f, green * f, blue * f)
  def /(f: Double): Spectrum = new Spectrum(red / f, green / f, blue / f)

  // Add another spectrum to this spectrum with a weight
  def +*(s: Spectrum, w: Double): Spectrum = new Spectrum(red + s.red * w, green + s.green * w, blue + s.blue * w)

  def isBlack = red == 0.0 && green == 0.0 && blue == 0.0

  def toRGB = (red, green, blue)

  override def toString = "Spectrum(red=%g, green=%g, blue=%g)" format (red, green, blue)
}

object Spectrum {
  // Spectrum constants
  val Black: Spectrum = new Spectrum(0.0, 0.0, 0.0) {
    override def +(s: Spectrum) = s
    override def *(s: Spectrum) = this
    override def *(f: Double) = this
    override def /(f: Double) = this
    override def +*(s: Spectrum, w: Double) = s * w
    override def isBlack = true

    override def toString = "Spectrum.Black"
  }

  val Unit: Spectrum = new Spectrum(1.0, 1.0, 1.0) {
    override def *(s: Spectrum) = s
    override def isBlack = false

    override def toString = "Spectrum.Unit"
  }

  // Create a new spectrum
  def apply(red: Double, green: Double, blue: Double) = new Spectrum(red, green, blue)
}
