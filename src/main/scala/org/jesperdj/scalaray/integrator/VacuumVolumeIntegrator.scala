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

import org.jesperdj.scalaray.sampler.Sample
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.RayDifferential

object VacuumVolumeIntegratorBuilder extends VolumeIntegratorBuilder {
  def build() = VacuumVolumeIntegrator
}

// Volume integrator that does nothing
object VacuumVolumeIntegrator extends VolumeIntegrator {
  // Compute the contribution of volume scattering along the ray; returns radiance and transmittance
  def radiance(ray: RayDifferential, sample: Sample): (Spectrum, Spectrum) = (Spectrum.Black, Spectrum.Unit)

  // Compute the fraction of light that is attenuated by volumetric scattering along the ray
  def transmittance(ray: RayDifferential, sample: Sample): Spectrum = Spectrum.Unit

  override def toString = "VacuumVolumeIntegrator"
}
