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
package org.jesperdj.scalaray.lightsource

import org.jesperdj.scalaray.common.{ Accumulator, RandomNumberGenerator }
import org.jesperdj.scalaray.sampler.{ Sample, SamplePatternSpec, SamplePatternSpec1D, SamplePatternSpec2D }
import org.jesperdj.scalaray.scene.Scene
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath._

import scala.collection.immutable.IndexedSeq

// Light sample (pbrt 14.6.1)
final case class LightSample (component: Double, u1: Double, u2: Double) {
  def this(rng: RandomNumberGenerator) = this(rng.nextDouble, rng.nextDouble, rng.nextDouble)
}

// Converter to transform sample patterns to LightSamples
final class LightSampleConverter (val numberOfSamples: Int, samplePatternSpecs: Accumulator[SamplePatternSpec]) {
  private val componentSamplePatternId = {
    val samplePatternSpec = new SamplePatternSpec1D(numberOfSamples)
    samplePatternSpecs += samplePatternSpec
    samplePatternSpec.id
  }

  private val positionSamplePatternId = {
    val samplePatternSpec = new SamplePatternSpec2D(numberOfSamples)
    samplePatternSpecs += samplePatternSpec
    samplePatternSpec.id
  }

  def lightSamples(sample: Sample): IndexedSeq[LightSample] = {
    val componentSamples = sample.samplePatterns1D(componentSamplePatternId)
    val positionSamples = sample.samplePatterns2D(positionSamplePatternId)

    componentSamples zip positionSamples map { case ((c, (u1, u2))) => new LightSample(c, u1, u2) }
  }
}

// Light source (pbrt 12.1)
trait LightSource {
  // Indicates whether the light is described by a delta distribution
  val isDeltaLight: Boolean

  // Number of samples to take from this light source
  val numberOfSamples: Int

  // Sample the incident radiance of this light source at the given point (pbrt 14.6.1)
  // Returns the radiance, a direction vector from the point to the light source, a ray from the light source to the given point (which can be used to
  // determine if the light is unoccluded) and the value of the probability density for this sample
  def sample(point: Point, sample: LightSample): (Spectrum, Vector, Ray, Double)

  // Probability density of the direction wi being sampled with respect to the distribution that sample uses (pbrt 14.6.1)
  def pdf(point: Point, wi: Vector): Double

  // Total emitted power of this light source onto the scene
  def totalPower(scene: Scene): Spectrum

  // Emitted radiance along a ray that does not intersect with geometry in the scene (for infinite area lights) (pbrt 12.5)
  def emittedRadiance(ray: Ray): Spectrum = Spectrum.Black
}
