/*
 * ScalaRay - Ray tracer based on pbrt (see http://pbrt.org) written in Scala 2.8
 * Copyright (C) 2009, 2010  Jesper de Jong
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

import org.jesperdj.scalaray.sampler.{ Sample, SampleSpec }
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.Ray

// Integrator (pbrt 16)
sealed abstract class Integrator {
	// Sample specifications for the sample patterns that this integrator needs
	val sampleSpecs: Traversable[SampleSpec]
}

// Surface integrator (pbrt 16)
abstract class SurfaceIntegrator extends Integrator {
	// Radiance along the ray (pbrt 16)
	def radiance(ray: Ray, sample: Sample): Spectrum
}

// Volume integrator (pbrt 17.2)
abstract class VolumeIntegrator extends Integrator {
	// Radiance along the ray; returns radiance and transmittance
	def radiance(ray: Ray, sample: Sample): (Spectrum, Spectrum)

	// Transmittance along the ray (pbrt 17.2)
	def transmittance(ray: Ray, sample: Sample): Spectrum
}
