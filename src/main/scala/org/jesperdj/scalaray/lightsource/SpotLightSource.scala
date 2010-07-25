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
import org.jesperdj.scalaray.spectrum._
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// Spot light source (pbrt 13.2.1)
final class SpotLightSource (position: Point, direction: Vector, falloffAngle: Float, widthAngle: Float, intensity: Spectrum) extends DeltaLightSource {
	require(widthAngle > falloffAngle, "Width angle must be greater than falloff angle for a spotlight")

	// Create a new spot light source using a transform to specify the position and direction
	def this(lightToWorld: Transform, falloffAngle: Float, widthAngle: Float, intensity: Spectrum) =
		this(lightToWorld * Point.Origin, (lightToWorld * Vector.ZAxis).normalize, falloffAngle, widthAngle, intensity)

	private val cosFalloff = math.cos(falloffAngle).toFloat
	private val cosWidth = math.cos(widthAngle).toFloat

	// Total emitted power of this light source onto the scene (pbrt 13.2.1)
	def totalPower(scene: Scene): Spectrum = intensity * (2.0f * π * (1.0f - 0.5f * (cosFalloff + cosWidth)))

	// Gets the incident radiance of this light source at the point (pbrt 13.2.1)
	// Returns the radiance and a ray from the light source to the point
	def incidentRadiance(point: Point): (Spectrum, Ray) = {
		// Ray direction vector from light source to intersection point
		var rd = point - position

		// Compute falloff factor
		val c = direction * rd.normalize
		val f = if (c < cosWidth) 0.0f else if (c > cosFalloff) 1.0f else {
			val d = (c - cosWidth) / (cosFalloff - cosWidth)
			d * d * d * d
		}

		// Compute radiance, attenuates by falloff factor and distance
		(if (f > 0.0f) intensity * (f / (rd.x * rd.x + rd.y * rd.y + rd.z * rd.z)) else Spectrum.Black, new Ray(position, rd, 0.0f, 1.0f))
	}

	override def toString = "SpotLightSource(position=%s, direction=%s, falloffAngle=%g, widthAndle=%g, intensity=%s)" format
		(position, direction, falloffAngle, widthAngle, intensity)
}
