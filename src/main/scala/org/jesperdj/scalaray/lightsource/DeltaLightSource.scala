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

import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath._

// Light source described by a delta distribution
trait DeltaLightSource extends LightSource {
  // Indicates whether the light is described by a delta distribution
  final val isDeltaLight: Boolean = true

  // Number of samples to take from this light source
  final val numberOfSamples: Int = 1

  // Radiance of this light source at the given point
  // Returns the radiance and a ray from the light source to the given point
  def radiance(point: Point): (Spectrum, Ray)

  // Sample the incident radiance of this light source at the given point (pbrt 14.6.1)
  // Returns the radiance, a direction vector from the point to the light source, a ray from the light source to the given point (which can be used to
  // determine if the light is unoccluded) and the value of the probability density for this sample
  final def sample(point: Point, sample: LightSample): (Spectrum, Vector, Ray, Double) = {
    val (rad, ray) = radiance(point)
    (rad, -ray.direction.normalize, ray, 1.0)
  }

  // Probability density of the direction wi being sampled with respect to the distribution that sample uses (pbrt 14.6.1)
  final def pdf(point: Point, wi: Vector): Double = 0.0
}
