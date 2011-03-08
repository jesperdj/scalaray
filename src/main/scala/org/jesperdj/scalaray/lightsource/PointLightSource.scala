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
package org.jesperdj.scalaray.lightsource

import org.jesperdj.scalaray.scene.Scene
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// Point light source (pbrt 12.2)
final class PointLightSource (position: Point, intensity: Spectrum) extends DeltaLightSource {
  // Create a point light source using a light-to-world transform
  def this(lightToWorld: Transform, intensity: Spectrum) = this(lightToWorld * Point.Origin, intensity)

  // Radiance of this light source at the given point
  // Returns the radiance and a ray from the light source to the given point
  def radiance(point: Point): (Spectrum, Ray) = {
    val rd = point - position
    (intensity / rd.lengthSquared, Ray(position, rd, 0.0, 1.0))
  }

  // Total emitted power of this light source onto the scene
  def totalPower(scene: Scene): Spectrum = intensity * (4.0 * π)

  override def toString = "PointLightSource(position=%s, intensity=%s)" format (position, intensity)
}
