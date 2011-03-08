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

// Distant light source (pbrt 12.3)
final class DistantLightSource (direction: Vector, radiance: Spectrum) extends DeltaLightSource {
  // Create a distant light source using a light-to-world transform
  def this(lightToWorld: Transform, radiance: Spectrum) = this(lightToWorld * Vector.ZAxis, radiance)

  // Radiance of this light source at the given point
  // Returns the radiance and a ray from the light source to the given point
  def radiance(point: Point): (Spectrum, Ray) = (radiance, Ray(point, direction, Double.NegativeInfinity, 0.0))

  // Total emitted power of this light source onto the scene
  def totalPower(scene: Scene): Spectrum = {
    val (center, radius) = scene.boundingSphere
    radiance * (π * radius * radius)
  }

  override def toString = "DistantLightSource(direction=%s, radiance=%s)" format (direction, radiance)
}
