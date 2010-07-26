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
package org.jesperdj.scalaray.renderer

import org.jesperdj.scalaray.camera.Camera
import org.jesperdj.scalaray.raster.Raster
import org.jesperdj.scalaray.sampler.Sample
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.Ray

// Renderer
abstract class Renderer {
	// Render a frame using the given camera and store the result in the given raster
	def render(camera: Camera, raster: Raster): Unit

	// Compute the radiance along the given ray; returns radiance and transmittance
	def radiance(ray: Ray, sample: Sample): (Spectrum, Spectrum)

	// Compute the transmittance along the given ray
	def transmittance(ray: Ray, sample: Sample): Spectrum
}