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
package org.jesperdj.scalaray.reflection

import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.Vector

// Specular transmission (pbrt 8.2.3)
final class SpecularTransmission extends BxDF {
  // BxDF type
  val bxdfType: BxDFType = BxDFType.Transmission | BxDFType.Specular

  // Evaluate the BxDF for the given pair of directions
  def apply(wo: Vector, wi: Vector): Spectrum = Spectrum.Black

  // Sample the BxDF for the given outgoing direction; returns reflectance or transmittance, incoming direction and value of the pdf
  override def sample(wo: Vector, u1: Double, u2: Double): (Spectrum, Vector, Double) = {
    // TODO: Implement SpecularTransmission.sample
    throw new UnsupportedOperationException("Not yet implemented")
  }

  // Probability density of the direction wi being sampled with respect to the distribution that sample uses
  override def pdf(wo: Vector, wi: Vector): Double = 0.0
}
