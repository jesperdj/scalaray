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
import org.jesperdj.scalaray.scene.Intersection
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.RayDifferential

import scala.collection.immutable.IndexedSeq

object SimpleSurfaceIntegratorBuilder extends SurfaceIntegratorBuilder {
  def build() = SimpleSurfaceIntegrator
}

// Simple surface integrator that colors each primitive with a color chosen from a fixed set
object SimpleSurfaceIntegrator extends SurfaceIntegrator {
  private val colors: IndexedSeq[Spectrum] = IndexedSeq(Spectrum(1.0, 0.0, 0.0), Spectrum(0.0, 1.0, 0.0),
    Spectrum(0.0, 0.0, 1.0), Spectrum(0.0, 1.0, 1.0), Spectrum(1.0, 0.0, 1.0), Spectrum(1.0, 1.0, 0.0))

  // Compute the incident radiance along the given ray
  def radiance(ray: RayDifferential, intersection: Intersection, sample: Sample): Spectrum =
    colors(intersection.primitive.hashCode % colors.size)

  override def toString = "SimpleSurfaceIntegrator"
}
