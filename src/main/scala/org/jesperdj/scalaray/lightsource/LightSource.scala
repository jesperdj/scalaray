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

import org.jesperdj.scalaray._
import org.jesperdj.scalaray.scene.Scene
import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.spectrum._
import org.jesperdj.scalaray.vecmath._

// Light source (pbrt 13.1)
sealed abstract class LightSource {
	// Total emitted power of this light source onto the scene (pbrt 13.1)
	def totalPower(scene: Scene): Spectrum
}

// Delta light source (pbrt 13.2)
abstract class DeltaLightSource extends LightSource {
	// Sample the radiance of this light source at the point (pbrt 13.1)
	// Returns the radiance and a ray from the light source to the point
	def sampleRadiance(point: Point): (Spectrum, Ray)
}

// Area light source (pbrt 13.4)
final class AreaLightSource (val shape: Shape, power: Spectrum, val numberOfSamplesX: Int, val numberOfSamplesY: Int) extends LightSource {
	// Total emitted power of this light source onto the scene (pbrt 13.4)
	def totalPower(scene: Scene): Spectrum = power * (shape.surfaceArea * π)

	// The area light's emitted radiance from a given point with the given normal on the surface of the light in the given direction (pbrt 13.4)
	def emittedRadiance(point: Point, normal: Normal, direction: Vector): Spectrum = if (normal * direction > 0.0) power else Spectrum.Black

	// Sample the radiance of this light source at the point using the random variables u1, u2 (pbrt 15.6.3)
	// Returns the radiance, a ray from light source to the point and the value of the probability distribution function for this sample
	def sampleRadiance(point: Point, u1: Double, u2: Double): (Spectrum, Ray, Double) = {
		// Sample a point on the surface of the area light with respect to the given point
		val (p, n, pdf) = shape.sampleSurface(point, u1, u2)

		// Point for shadow ray calculations just above light surface to avoid self-intersection
		val lightPoint = p + n * 1e-6

		// Ray direction from point on light source to given point
		val rd = point - lightPoint

		// Return the radiance only if the light shines from the right side of the surface of the light source
		(if (n * rd > 0.0) power else Spectrum.Black, new Ray(lightPoint, rd, 0.0, 1.0), pdf)
	}

	override def toString = "AreaLightSource(shape=%s, power=%s)" format (shape, power)
}
