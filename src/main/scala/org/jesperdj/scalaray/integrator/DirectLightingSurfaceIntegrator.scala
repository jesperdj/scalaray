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
package org.jesperdj.scalaray.integrator

import org.jesperdj.scalaray.common.Accumulator
import org.jesperdj.scalaray.sampler.{ Sample, SamplePatternSpec, SamplePatternSpec1D, SamplePatternSpec2D }
import org.jesperdj.scalaray.scene.Intersection
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath._
import org.jesperdj.scalaray.lightsource.{AreaLightSource, LightSample, LightSampleConverter, LightSource}
import org.jesperdj.scalaray.reflection.{BxDFType, BsdfSample, BsdfSampleConverter, BSDF}

// Builder for DirectLightingSurfaceIntegrator
final class DirectLightingSurfaceIntegratorBuilder extends SurfaceIntegratorBuilder {
  private var samplePatternSpecs: Option[Accumulator[SamplePatternSpec]] = None

  def withSamplePatternSpecs(arg: Accumulator[SamplePatternSpec]) = { samplePatternSpecs = Some(arg); this }

  def build() = {
    if (samplePatternSpecs.isEmpty) throw new IllegalArgumentException("Setting samplePatternSpecs is required")
    if (integrator.isEmpty) throw new IllegalArgumentException("Setting integrator is required")
    new DirectLightingSurfaceIntegrator(samplePatternSpecs.get, integrator.get)
  }
}

// Direct lighting surface integrator (pbrt 15.1)
final class DirectLightingSurfaceIntegrator (samplePatternSpecs: Accumulator[SamplePatternSpec], integrator: Integrator) extends SurfaceIntegrator {
  // Sample converters for a light source
  private final class SampleConverters (val lightSource: LightSource) {
    val count = lightSource.numberOfSamples // TODO: Could be 1 for different sampling strategy
    val lightSampleConverter = new LightSampleConverter(count, samplePatternSpecs)
    val bsdfSampleConverter = new BsdfSampleConverter(count, samplePatternSpecs)
  }

  // Sample converters for all light sources
  private val sampleConverters = integrator.scene.lightSources map { new SampleConverters(_) }

  // Compute the incident radiance along the given ray
  def radiance(ray: RayDifferential, intersection: Intersection, sample: Sample): Spectrum = {
    val wo = -ray.direction.normalize

    // Get emitted radiance if intersection point is on an area light source
    val emitted = intersection.emittedRadiance(wo)

    // TODO: This does not work if the light source is on the wrong side of the surface. Solve the self-intersection problem differently.
    // Point for shadow ray calculations just above surface to avoid self-intersection
    val dg = intersection.dg
    val shadowPoint = dg.point + dg.normal * 1e-6

    // Get radiance of direct light from light sources on the intersection point
    val direct = uniformSampleAllLights(shadowPoint, dg.normal, wo, intersection.bsdf, sample)

    // TODO: trace rays for specular reflection and refraction (make recursion stop at a depth limit)

    emitted + direct
  }

  private def uniformSampleAllLights(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum = {
    // Accumulate contributions of all light sources
    sampleConverters.foldLeft(Spectrum.Black) { (directLight, sc) =>
      val lightSamples = sc.lightSampleConverter.lightSamples(sample)
      val bsdfSamples = sc.bsdfSampleConverter.bsdfSamples(sample)

      // Estimate direct light contribution of the current light source
      val estimateTotal = (lightSamples zip bsdfSamples).foldLeft(Spectrum.Black) { case (total, (lightSample, bsdfSample)) =>
        total + estimateDirect(sc.lightSource, point, normal, wo, bsdf, lightSample, bsdfSample)
      }

      directLight + estimateTotal / sc.count
    }
  }

  private def uniformSampleOneLight(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum = {
    // TODO: Implement uniformSampleOneLight
    throw new UnsupportedOperationException("Not yet implemented")
  }

  private def estimateDirect(lightSource: LightSource, point: Point, normal: Normal, wo: Vector, bsdf: BSDF,
                             lightSample: LightSample, bsdfSample: BsdfSample): Spectrum = {
    var result = Spectrum.Black

    // TODO: niet handig dat we steeds BxDFType.All & ~BxDFType.Specular moeten doorgeven of checken of dat bit gezet is
    // Eigenlijk zou BxDF / BSDF dat soort details moeten verbergen; we moeten alleen weten of het wel of niet een BxDF is met een delta-distributie

    // Sample light source with multiple importance sampling
    val (radiance, ray, lightPdf) = lightSource.sampleRadiance(point, lightSample)
    if (lightPdf > 0.0 && !radiance.isBlack) {
      val wi = -ray.direction.normalize

      // Evaluate BSDF for the chosen direction
      val reflectance = bsdf(wo, wi, BxDFType.All & ~BxDFType.Specular)

      if (!reflectance.isBlack && !integrator.scene.checkIntersect(ray)) {
        // TODO: take transmittance along ray into account

        val weight = if (lightSource.isDeltaLight) 1.0 else powerHeuristic(1, lightPdf, 1, bsdf.pdf(wo, wi, BxDFType.All & ~BxDFType.Specular))
        result +*= (radiance * reflectance, (wi * normal).abs * weight / lightPdf)
      }
    }

    // Sample BSDF with multiple importance sampling
    if (!lightSource.isDeltaLight) {
      val (reflectance, wi, bsdfPdf, sampledType) = bsdf.sample(wo, bsdfSample, BxDFType.All & ~BxDFType.Specular)

      if (bsdfPdf > 0.0 && !reflectance.isBlack) {
        val lightPdf = lightSource.pdf(point, wi)
        if (lightPdf > 0.0) {
          // Evaluate radiance of light source at the point from the chosen direction
          val radiance = integrator.scene.intersect(Ray(point, wi)) match {
            case Some(Intersection(dg, prim, _)) if (prim.areaLightSource.isDefined && prim.areaLightSource.get == lightSource) =>
              // Ray intersects with this area light source and point isn't shadowed
              lightSource.asInstanceOf[AreaLightSource].emittedRadiance(dg.point, dg.normal, -wi)

            case None => // No intersection
              // Get emitted radiance along the ray, for infinite area light sources
              lightSource.emittedRadiance(ray)

            case _ => // Point is shadowed
              Spectrum.Black
          }

          if (!radiance.isBlack) {
            // TODO: take transmittance along ray into account

            val weight = if ((sampledType & BxDFType.Specular) == BxDFType.Specular) 1.0 else powerHeuristic(1, bsdfPdf, 1, lightPdf)
            result +*= (radiance * reflectance, (wi * normal).abs * weight / bsdfPdf)
          }
        }
      }
    }

    result
  }

  // Balance heuristic weighing function for multiple importance sampling (pbrt 14.4.1)
  private def balanceHeuristic(nf: Int, fPdf: Double, ng: Int, gPdf: Double): Double =
    (nf * fPdf) / (nf * fPdf + ng * gPdf)

  // Power heuristic weighing function with β = 2 for multiple importance sampling (pbrt 14.4.1)
  private def powerHeuristic(nf: Int, fPdf: Double, ng: Int, gPdf: Double): Double = {
    val f = nf * fPdf
    val g = ng * gPdf
    (f * f) / (f * f + g * g)
  }

  private def specularReflect(): Spectrum = {
    // TODO: Implement specularReflect
    throw new UnsupportedOperationException("Not yet implemented")
  }

  private def specularTransmit: Spectrum = {
    // TODO: Implement specularTransmit
    throw new UnsupportedOperationException("Not yet implemented")
  }

  override def toString = "DirectLightingSurfaceIntegrator"
}
