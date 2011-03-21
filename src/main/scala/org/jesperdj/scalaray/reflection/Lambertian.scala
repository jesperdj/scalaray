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

import scala.collection.immutable.Traversable

import org.jesperdj.scalaray.common._
import org.jesperdj.scalaray.sampler._
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.Vector

// Lambertian reflection (pbrt 8.3)
final class Lambertian (reflectance: Spectrum) extends BxDF {
  // BxDF type
  val bxdfType: BxDFType = BxDFType.Reflection | BxDFType.Diffuse

  // Evaluate the BxDF for the given pair of directions
  def apply(wo: Vector, wi: Vector): Spectrum = reflectance / π

  // Compute hemispherical-directional reflectance
  override def rho(wo: Vector, samples: SamplePattern2D): Spectrum = reflectance

  // Compute hemispherical-hemispherical reflectance
  override def rho(samples1: SamplePattern2D, samples2: SamplePattern2D): Spectrum = reflectance
}
