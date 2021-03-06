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

import org.jesperdj.scalaray.common.Builder
import org.jesperdj.scalaray.sampler.Sample
import org.jesperdj.scalaray.scene.{ Intersection, Scene }
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.Ray

// Surface integrator (pbrt 15)
trait SurfaceIntegrator {
  // Compute the incident radiance along the given ray
  def radiance(ray: Ray, intersection: Intersection, sample: Sample): Spectrum
}

// Volume integrator (pbrt 16.2)
trait VolumeIntegrator {
  // Compute the contribution of volume scattering along the ray; returns radiance and transmittance
  def radiance(ray: Ray, sample: Sample): (Spectrum, Spectrum)

  // Compute the fraction of light that is attenuated by volumetric scattering along the ray
  def transmittance(ray: Ray, sample: Sample): Spectrum
}

trait WithIntegrator {
  protected var integrator: Option[Integrator] = None

  def withIntegrator(arg: Integrator): this.type = { integrator = Some(arg); this }
}

trait SurfaceIntegratorBuilder extends Builder[SurfaceIntegrator] with WithIntegrator
trait VolumeIntegratorBuilder extends Builder[VolumeIntegrator] with WithIntegrator

// NOTE: Integrator in ScalaRay is not the same thing as Integrator in pbrt. Here, the Integrator is the object that
// combines a SurfaceIntegrator and VolumeIntegrator to compute the incident radiance along a ray. In pbrt, this
// functionality is implemented in Renderer.

// Integrator; combines a SurfaceIntegrator and VolumeIntegrator
final class Integrator (val scene: Scene, surfaceIntegratorBuilder: SurfaceIntegratorBuilder, volumeIntegratorBuilder: VolumeIntegratorBuilder) {
  private val surfaceIntegrator = surfaceIntegratorBuilder.withIntegrator(this).build()
  private val volumeIntegrator = volumeIntegratorBuilder.withIntegrator(this).build()

  // Compute the incident radiance along the given ray
  def radiance(ray: Ray, sample: Sample): Spectrum = {
    val li = scene.intersect(ray) match {
      // If the ray intersects geometry in the scene, get the reflected radiance from the surface integrator
      case Some(intersection) => surfaceIntegrator.radiance(ray, intersection, sample)

      // If the ray does not intersect any geometry, accumulate the contributions of infinite area light sources along the ray
      case _ => scene.lightSources.foldLeft(Spectrum.Black) { (acc, ls) => acc + ls.emittedRadiance(ray) }
    }

    val (lvi, t) = volumeIntegrator.radiance(ray, sample)

    t * li + lvi
  }

  // Compute the fraction of light that is attenuated by volumetric scattering along the ray
  def transmittance(ray: Ray, sample: Sample): Spectrum = volumeIntegrator.transmittance(ray, sample)
}
