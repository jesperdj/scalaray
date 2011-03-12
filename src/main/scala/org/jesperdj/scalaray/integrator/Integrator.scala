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

import org.jesperdj.scalaray.renderer.Renderer
import org.jesperdj.scalaray.sampler.Sample
import org.jesperdj.scalaray.scene.Intersection
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.RayDifferential

// TODO: Refactor this.
// Integrators should not know the Renderer, see comments in Renderer.scala
// The Renderer (or RayTracer) should not be passed at all in the radiance() and transmittance() methods. Whether the integrator needs these things is
// an implementation detail of the integrator, so it should be left to specific implementations to get these as for example constructor parameters.

// Integrator (pbrt 15)
sealed trait Integrator

// Surface integrator (pbrt 15)
trait SurfaceIntegrator extends Integrator {
  // Compute the incident radiance along the given ray
  def radiance(renderer: Renderer, ray: RayDifferential, intersection: Intersection, sample: Sample): Spectrum
}

// Volume integrator (pbrt 16.2)
trait VolumeIntegrator extends Integrator {
  // TODO: Description
  def radiance(renderer: Renderer, ray: RayDifferential, sample: Sample): (Spectrum, Spectrum)

  // TODO: Description
  def transmittance(renderer: Renderer, ray: RayDifferential, sample: Sample): Spectrum
}
