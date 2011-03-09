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
package org.jesperdj.scalaray.renderer

import org.jesperdj.scalaray.sampler.Sample
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.RayDifferential

// TODO: Refactor this. Separate rendering (which includes producing an image) from ray tracing.
// Renderer should be the thing that gets a sampler, a camera and that outputs a PixelBuffer. It runs a job for each sample batch.
// In each job, the camera is called to create a ray for each sample. Th RayTracer is used to compute the radiance for this ray.
// The radiance is added to the PixelBuffer.
// RayTracer should be the thing that gets the scene and integrators and that can trace rays and compute radiance and transmittance.
// RayTracer does not know anything about the sampler, camera and PixelBuffer.
// Integrators that do recursive ray tracing can get a reference to the RayTracer - NOT to the Renderer, as it is now.

// Renderer (pbrt 1.3.3)
abstract class Renderer {
  // Render the scene
  def render(): Unit

  // Compute the incident radiance along the given ray
  def radiance(ray: RayDifferential, sample: Sample): Spectrum

  // Compute the fraction of light that is attenuated by volumetric scattering along the ray
  def transmittance(ray: RayDifferential, sample: Sample): Spectrum
}
