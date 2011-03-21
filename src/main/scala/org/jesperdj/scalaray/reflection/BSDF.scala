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

import scala.collection.immutable.IndexedSeq

import org.jesperdj.scalaray.common.Accumulator
import org.jesperdj.scalaray.sampler.{ Sample, SamplePatternSpec, SamplePatternSpec1D, SamplePatternSpec2D }
import org.jesperdj.scalaray.shape.DifferentialGeometry
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.{ Normal, Vector }

// BSDF sample (pbrt 14.5.6)
final case class BSDFSample (component: Double, u1: Double, u2: Double)

// Converter to transform sample patterns to BSDFSamples
final class BSDFSampleConverter (val numberOfSamples: Int, samplePatternSpecs: Accumulator[SamplePatternSpec]) {
  private val componentSamplePatternId = {
    val samplePatternSpec = new SamplePatternSpec1D(numberOfSamples)
    samplePatternSpecs += samplePatternSpec
    samplePatternSpec.id
  }

  private val directionSamplePatternId = {
    val samplePatternSpec = new SamplePatternSpec2D(numberOfSamples)
    samplePatternSpecs += samplePatternSpec
    samplePatternSpec.id
  }

  def bsdfSamples(sample: Sample): IndexedSeq[BSDFSample] = {
    val componentSamples = sample.samplePatterns1D(componentSamplePatternId)
    val directionSamples = sample.samplePatterns2D(directionSamplePatternId)

    componentSamples zip directionSamples map { case ((c, (u1, u2))) => new BSDFSample(c, u1, u2) }
  }
}

// Bidirectional Scattering Distribution Function (pbrt 9.1)
final class BSDF (bxdfs: IndexedSeq[BxDF], dgShading: DifferentialGeometry, ng: Normal, eta: Double = 1.0) {
  private val nn = dgShading.normal
  private val sn = dgShading.dpdu.normalize
  private val tn = nn ** sn

  // Transform a vector from world coordinates to the local BxDF coordinate system
  private def worldToLocal(v: Vector) = Vector(v * sn, v * tn, v * nn)

  // Transform a vector from the local BxDF coordinate system to world coordinates
  private def localToWorld(v: Vector) = Vector(
    sn.x * v.x + tn.x * v.y + nn.x * v.z,
    sn.y * v.x + tn.y * v.y + nn.y * v.z,
    sn.z * v.x + tn.z * v.y + nn.z * v.z)

  // Find BxDFs that match a type mask
  private def matchBxDFs(typeMask: BxDFType): IndexedSeq[BxDF] = bxdfs filter { _.bxdfType.matches(typeMask) }

  // Evaluate components of the BSDF that match a type mask for the given pair of directions (pbrt 9.1)
  def apply(woW: Vector, wiW: Vector, typeMask: BxDFType = BxDFType.All): Spectrum = {
    val wo = worldToLocal(woW)
    val wi = worldToLocal(wiW)

    // Select BRDFs or BTDFs depending on the geometry
    val mask = if ((wiW * ng) * (woW * ng) > 0.0) typeMask & ~BxDFType.Transmission else typeMask & ~BxDFType.Reflection

    // Accumulate contributions of BxDFs that match the mask
    matchBxDFs(mask).foldLeft(Spectrum.Black) { (total, bxdf) => total + bxdf(wo, wi) }
  }

  // Sample the BSDF for the given outgoing direction; returns reflectance or transmittance, incoming direction, value of the pdf and
  // type of the selected BxDF component (pbrt 14.5.6)
  def sample(woW: Vector, sample: BSDFSample, typeMask: BxDFType = BxDFType.All): (Spectrum, Vector, BxDFType, Double) = {
    // Get BxDFs that match the mask
    val matchingBxDFs = matchBxDFs(typeMask)
    if (matchingBxDFs.size == 0) return (Spectrum.Black, Vector.Zero, BxDFType.None, 0.0)

    // Select the BxDF to sample
    val bxdf = matchingBxDFs(math.min((sample.component * matchingBxDFs.size).floor.toInt, matchingBxDFs.size - 1))

    // Sample the selected BxDF
    val wo = worldToLocal(woW)
    val (sampledSpectrum, wi, sampledPdf) = bxdf.sample(wo, sample.u1, sample.u2)
    if (sampledPdf == 0.0) return (Spectrum.Black, Vector.Zero, BxDFType.None, 0.0)
    val wiW = localToWorld(wi)

    // Compute the overall pdf with all matching BxDFs
    val pdf = (if (bxdf.bxdfType.isDeltaBxDF || matchingBxDFs.size == 1) sampledPdf else
      (sampledPdf + (matchingBxDFs.view filter { _ != bxdf } map { _.pdf(wo, wi) } sum))) / matchingBxDFs.size

    // Compute value of BSDF for sampled direction
    val spectrum = if (bxdf.bxdfType.isDeltaBxDF) sampledSpectrum else {
      // Select BRDFs or BTDFs depending on the geometry
      val mask = if ((wiW * ng) * (woW * ng) > 0.0) typeMask & ~BxDFType.Transmission else typeMask & ~BxDFType.Reflection

      // Accumulate contributions of BxDFs that match the mask
      matchBxDFs(mask).foldLeft(Spectrum.Black) { (total, bxdf) => total + bxdf(wo, wi) }
    }

    (spectrum, wiW, bxdf.bxdfType, pdf)
  }

  // Probability density of the direction wiW being sampled with respect to the distribution that sample uses (pbrt 14.5.6)
  def pdf(woW: Vector, wiW: Vector, typeMask: BxDFType = BxDFType.All): Double = {
    if (bxdfs.size == 0) return 0.0

    val wo = worldToLocal(woW)
    val wi = worldToLocal(wiW)

    val matchingBxDFs = matchBxDFs(typeMask)
    if (matchingBxDFs.size == 0) return 0.0

    // Compute average of the pdfs of the matching BxDF components
    (matchingBxDFs.view map { _.pdf(wo, wi) } sum) / matchingBxDFs.size
  }
}
