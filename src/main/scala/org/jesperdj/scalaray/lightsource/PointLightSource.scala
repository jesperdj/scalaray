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

// Point light source (pbrt 13.2)
final class PointLightSource (position: Point, intensity: Spectrum) extends DeltaLightSource {
	// Create a new point light source using a transform to specify the position
	def this(lightToWorld: Transform, intensity: Spectrum) = this(lightToWorld * Point.Origin, intensity)

	// Total emitted power of this light source onto the scene (pbrt 13.2)
	def totalPower(scene: Scene): Spectrum = intensity * (4.0f * π)

	// Gets the incident radiance of this light source at the point (pbrt 13.2)
	// Returns the radiance and a ray from the light source to the point
	def incidentRadiance(point: Point): (Spectrum, Ray) = {
		var rd = point - position
		(intensity / rd.lengthSquared, new Ray(position, rd, 0.0f, 1.0f))
	}

	override def toString = "PointLightSource(position=%s, intensity=%s)" format (position, intensity)
}
