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

import org.jesperdj.scalaray.common._
import org.jesperdj.scalaray.sampler._
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.Vector

// BxDF type (pbrt 8.1)
final class BxDFType private (val bits: Int) {
  def &(other: BxDFType): BxDFType = new BxDFType(bits & other.bits)
  def |(other: BxDFType): BxDFType = new BxDFType(bits | other.bits)

  def unary_~ = new BxDFType(~bits)

  // Check if this BxDFType matches the mask
  def matches(mask: BxDFType): Boolean = (this.bits & mask.bits) == this.bits

  // Indicates whether the BxDF is described by a delta distribution
  def isDeltaBxDF = (this & BxDFType.Specular) == BxDFType.Specular

  override def equals(other: Any): Boolean = other match {
    case that: BxDFType => (that canEqual this) && (bits == that.bits)
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[BxDFType]

  override def hashCode: Int = bits
}

object BxDFType {
  val None = new BxDFType(0)

  val Reflection = new BxDFType(1 << 0)
  val Transmission = new BxDFType(1 << 1)

  val Diffuse = new BxDFType(1 << 2)
  val Glossy = new BxDFType(1 << 3)
  val Specular = new BxDFType(1 << 4)

  val AllTypes = Diffuse | Glossy | Specular
  val AllReflection = Reflection | AllTypes
  val AllTransmission = Transmission | AllTypes
  val All = AllReflection | AllTransmission
}

// Bidirectional Reflectance or Transmittance Distribution Function (pbrt 8.1)
trait BxDF {
  // BxDF type
  val bxdfType: BxDFType

  // Evaluate the BxDF for the given pair of directions
  def apply(wo: Vector, wi: Vector): Spectrum

  // Sample the BxDF for the given outgoing direction; returns reflectance or transmittance, incoming direction and value of the pdf (pbrt 14.5)
  def sample(wo: Vector, u1: Double, u2: Double): (Spectrum, Vector, Double) = {
    val wi = { val p = SampleTransforms.cosineSampleHemisphere(u1, u2); if (wo.z >= 0.0) Vector(p) else Vector(p.x, p.y, -p.z) }
    (apply(wo, wi), wi, pdf(wo, wi))
  }

  // Probability density of the direction wi being sampled with respect to the distribution that sample uses (pbrt 14.5)
  def pdf(wo: Vector, wi: Vector): Double = if (wo.z * wi.z > 0.0) SampleTransforms.cosineHemispherePdf(wi) else 0.0

  // Compute hemispherical-directional reflectance (pbrt 8.1.1, 14.5.5)
  def rho(wo: Vector, samples: SamplePattern2D): Spectrum =
    throw new UnsupportedOperationException("Not yet implemented") // TODO: Implement BxDF.rho

  // Compute hemispherical-hemispherical reflectance (pbrt 8.1.1, 14.5.5)
  def rho(samples1: SamplePattern2D, samples2: SamplePattern2D): Spectrum =
    throw new UnsupportedOperationException("Not yet implemented") // TODO: Implement BxDF.rho
}
