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
import org.jesperdj.scalaray.sampler.{ Sample, SamplePatternSpec }
import org.jesperdj.scalaray.scene.{ Intersection, Scene }
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.RayDifferential

// Direct lighting surface integrator (pbrt 15.1)
final class DirectLightingSurfaceIntegrator (scene: Scene, samplePatternSpecs: Accumulator[SamplePatternSpec]) extends SurfaceIntegrator {
  // Compute the incident radiance along the given ray
  def radiance(ray: RayDifferential, intersection: Intersection, sample: Sample, integrator: Integrator): Spectrum = {
    // TODO: Implement DirectLightingSurfaceIntegrator
    throw new UnsupportedOperationException("Not yet implemented")
  }

  override def toString = "DirectLightingSurfaceIntegrator"
}
