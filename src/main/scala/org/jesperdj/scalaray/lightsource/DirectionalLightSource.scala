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

// Directional light source (pbrt 12.3)
final class DirectionalLightSource (direction: Vector, intensity: Spectrum) extends DeltaLightSource {
	// Create a new directional light source using a transform to specify the direction
	def this(lightToWorld: Transform, intensity: Spectrum) = this(lightToWorld * Vector.ZAxis, intensity)

	// Total emitted power of this light source onto the scene
	def totalPower(scene: Scene): Spectrum = {
		val (p, r) = scene.boundingSphere
		intensity * (π * r * r)
	}

	// Gets the incident radiance of this light source at the point
	// Returns the radiance and a ray from the light source to the point
	def incidentRadiance(point: Point): (Spectrum, Ray) = (intensity, new Ray(point, direction, Float.NegativeInfinity, 0.0f))

	override def toString = "DirectionalLightSource(direction=%s, intensity=%s)" format (direction, intensity)
}
