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

import scala.collection.immutable.Traversable

import org.jesperdj.scalaray.renderer.Renderer
import org.jesperdj.scalaray.sampler.{ Sample, SamplePatternSpec }
import org.jesperdj.scalaray.scene.Intersection
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.RayDifferential

// TODO: Refactor this. Integrators should not have a val sampleSpecs.
// Instead, an Accumulator[SamplePatternSpec] should be passed to the constructor of integrators that need this. The constructor of the integrator
// should add SamplePatternSpec instances to this accumulator if it needs additional sample patterns.

// Integrator (pbrt 15)
sealed abstract class Integrator {
  // Sample specifications for the sample patterns that this integrator needs
  val sampleSpecs: Traversable[SamplePatternSpec]
}

// Surface integrator (pbrt 15)
abstract class SurfaceIntegrator extends Integrator {
  // Compute the incident radiance along the given ray
  def radiance(renderer: Renderer, ray: RayDifferential, intersection: Intersection, sample: Sample): Spectrum
}

// Volume integrator (pbrt 16.2)
abstract class VolumeIntegrator extends Integrator {
  // TODO
  def radiance(renderer: Renderer, ray: RayDifferential, sample: Sample): (Spectrum, Spectrum)

  // TODO
  def transmittance(renderer: Renderer, ray: RayDifferential, sample: Sample): Spectrum
}
