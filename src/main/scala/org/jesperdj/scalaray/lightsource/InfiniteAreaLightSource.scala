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
package org.jesperdj.scalaray.lightsource

import org.jesperdj.scalaray.scene.Scene
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath._

// Infinite area light source (pbrt 12.5)
final class InfiniteAreaLightSource (val numberOfSamples: Int) extends LightSource {
	// Indicates whether the light is described by a delta distribution
	val isDeltaLight: Boolean = false

	// Sample the incident radiance of this light source at the given point (pbrt 14.6.1)
	// Returns the radiance, a ray from the light source to the given point and the value of the probability density for this sample
	def sampleRadiance(point: Point, u1: Float, u2: Float): (Spectrum, Ray, Float) =
		throw new UnsupportedOperationException("Not yet implemented") // TODO

	// Probability density of the direction wi (from the given point to a point on the light source) being sampled with respect to the distribution
	// that sampleRadiance(point: Point, u1: Float, u2: Float) uses to sample points (pbrt 14.6.1)
	def pdf(point: Point, wi: Vector): Float =
		throw new UnsupportedOperationException("Not yet implemented") // TODO

	// Total emitted power of this light source onto the scene
	def totalPower(scene: Scene): Spectrum =
		throw new UnsupportedOperationException("Not yet implemented") // TODO

	// Emitted radiance along a ray that does not intersect with geometry in the scene (for infinite area lights) (pbrt 12.5)
	override def emittedRadiance(ray: Ray): Spectrum =
		throw new UnsupportedOperationException("Not yet implemented") // TODO

	override def toString = "InfiniteAreaLightSource(numberOfSamples=%d)" format (numberOfSamples) // TODO
}
