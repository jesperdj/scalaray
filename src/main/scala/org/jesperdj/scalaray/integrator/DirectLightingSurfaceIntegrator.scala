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
import org.jesperdj.scalaray.lightsource.{ LightSample, LightSampleConverter, LightSource }
import org.jesperdj.scalaray.sampler.{ Sample, SamplePatternSpec, SamplePatternSpec1D, SamplePatternSpec2D }
import org.jesperdj.scalaray.scene.Intersection
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.{ Normal, Point, RayDifferential, Vector }
import org.jesperdj.scalaray.reflection.{BsdfSample, BsdfSampleConverter, BSDF}

final class DirectLightingSurfaceIntegratorBuilder extends SurfaceIntegratorBuilder {
  private var samplePatternSpecs: Option[Accumulator[SamplePatternSpec]] = None
  private var integrator: Option[Integrator] = None

  def withSamplePatternSpecs(arg: Accumulator[SamplePatternSpec]) = { samplePatternSpecs = Some(arg); this }
  override def withIntegrator(arg: Integrator) = { integrator = Some(arg); this }

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
    private val count = lightSource.numberOfSamples // TODO: Could be 1 for different sampling strategy
    val lightSampleConverter = new LightSampleConverter(count, samplePatternSpecs)
    val bsdfSampleConverter = new BsdfSampleConverter(count, samplePatternSpecs)
  }

  private val sampleConverters = integrator.scene.lightSources map { new SampleConverters(_) }

  // Compute the incident radiance along the given ray
  def radiance(ray: RayDifferential, intersection: Intersection, sample: Sample): Spectrum = {
    // TODO: Implement DirectLightingSurfaceIntegrator
    throw new UnsupportedOperationException("Not yet implemented")
  }

  private def uniformSampleAllLights(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum = {
    // TODO: Implement uniformSampleAllLights
    throw new UnsupportedOperationException("Not yet implemented")
  }

  private def uniformSampleOneLight(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum = {
    // TODO: Implement uniformSampleOneLight
    throw new UnsupportedOperationException("Not yet implemented")
  }

  private def estimateDirect(lightSource: LightSource, point: Point, normal: Normal, wo: Vector, bsdf: BSDF, lightSample: LightSample, bsdfSample: BsdfSample): Spectrum = {
    // TODO: Implement estimateDirect
    throw new UnsupportedOperationException("Not yet implemented")
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
