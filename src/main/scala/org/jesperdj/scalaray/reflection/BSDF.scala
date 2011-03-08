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

import org.jesperdj.scalaray.shape.DifferentialGeometry
import org.jesperdj.scalaray.spectrum._
import org.jesperdj.scalaray.common._
import org.jesperdj.scalaray.vecmath._

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

  // Evaluate components of the BSDF that match the given type for the given pair of directions (pbrt 9.1)
  def apply(woW: Vector, wiW: Vector, bxdfType: BxDFType = BxDFType.All): Spectrum = {
    // Select BRDFs or BTDFs depending on the geometry
    val flags = if ((wiW * ng) * (woW * ng) > 0.0) bxdfType & ~BxDFType.Transmission else bxdfType & ~BxDFType.Reflection

    val wo = worldToLocal(woW); val wi = worldToLocal(wiW)

    // Accumulate contributions of BxDFs that match the flags
    (Spectrum.Black /: (bxdfs filter (_.matchesType(flags)))) { (accu, bxdf) => accu + bxdf(wo, wi) }
  }

  // Sample the BSDF for the given outgoing direction; returns reflectance or transmittance, incoming direction,
  // value of the pdf and type of the selected BxDF component (pbrt 14.5.6)
  def sample(woW: Vector, u1: Double, u2: Double, u3: Double, bxdfType: BxDFType = BxDFType.All): (Spectrum, Vector, Double, BxDFType) = {
    // Get BxDFs that match the given type
    val matchingBxDFs = bxdfs filter (_.matchesType(bxdfType))
    if (matchingBxDFs.size == 0) return (Spectrum.Black, Vector.Zero, 0.0, BxDFType.None)

    // Get the BxDF to sample
    val bxdf = matchingBxDFs(math.min((u3 * matchingBxDFs.size).floor.toInt, matchingBxDFs.size - 1))

    // Sample the selected BxDF
    val wo = worldToLocal(woW)
    val (spec, wi, pdf) = bxdf.sample(wo, u1, u2)
    if (pdf == 0.0) return (Spectrum.Black, Vector.Zero, 0.0, BxDFType.None)
    val wiW = localToWorld(wi)

    // Compute the overall pdf with all matching BxDFs
    var totalPdf = pdf
    if (!bxdf.matchesType(BxDFType.Specular) && matchingBxDFs.size > 1)
      for (b <- matchingBxDFs if (b != bxdf)) totalPdf += b.pdf(wo, wi)
    if (matchingBxDFs.size > 1) totalPdf /= matchingBxDFs.size

    // Compute value of BSDF for sampled direction
    val spectrum = if (bxdf.matchesType(BxDFType.Specular)) spec
    else {
      // Select BRDFs or BTDFs depending on the geometry
      val flags = if ((wiW * ng) * (woW * ng) > 0.0) bxdfType & ~BxDFType.Transmission else bxdfType & ~BxDFType.Reflection

      // Accumulate contributions of BxDFs that match the flags
      (Spectrum.Black /: (bxdfs filter (_.matchesType(flags)))) { (accu, b) => accu + b(wo, wi) }
    }

    (spectrum, wiW, totalPdf, bxdf.bxdfType)
  }

  // Get the value of the probability distribition function that matches the sampling method of sample(Vector, Double, Double, Double, BxDFType) (pbrt 14.5.6)
  def pdf(woW: Vector, wiW: Vector, bxdfType: BxDFType = BxDFType.All): Double = {
    if (bxdfs.size == 0) return 0.0

    val wo = worldToLocal(woW); val wi = worldToLocal(wiW)

    val matchingBxDFs = bxdfs filter (_.matchesType(bxdfType))
    if (matchingBxDFs.size == 0.0) return 0.0

    // Compute average of the pdfs of the matching BxDF components
    ((0.0 /: matchingBxDFs) { (accu, bxdf) => accu + bxdf.pdf(wo, wi) }) / matchingBxDFs.size
  }
}
