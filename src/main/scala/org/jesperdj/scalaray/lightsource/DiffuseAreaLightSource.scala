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
import org.jesperdj.scalaray.shape.Shape
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// Diffuse area light source (pbrt 12.4)
final class DiffuseAreaLightSource (shape: Shape, lightToWorld: Transform, emitted: Spectrum, val numberOfSamples: Int) extends AreaLightSource {
  require(!lightToWorld.hasScale, "DiffuseAreaLightSource requires that the light-to-world transform has no scale factor")

  private val worldToLight = lightToWorld.inverse

  // Sample the incident radiance of this light source at the given point (pbrt 14.6.1)
  // Returns the radiance, a ray from the light source to the given point and the value of the probability density for this sample
  def sampleRadiance(point: Point, u1: Float, u2: Float): (Spectrum, Ray, Float) = {
    // Sample a point on the light source with respect to the given point
    val (sp, sn, pdf) = shape.sampleSurface(worldToLight * point, u1, u2)
    if (pdf == 0.0f) return (Spectrum.Black, Ray(Point.Origin, Vector.ZAxis, 0.0f, 0.0f), 0.0f)

    // Transform point and normal to world coordinates
    val p = lightToWorld * sp
    val n = lightToWorld * sn

    // Ray direction from point on light source to given point
    val rd = point - p

    // Return the radiance only if the light shines from the right side of the surface of the light source
    (emittedRadiance(p, n, rd), new Ray(p, rd, 1e-3f, 1.0f), pdf)
  }

  // Probability density of the direction wi (from the given point to a point on the light source) being sampled with respect to the distribution
  // that sampleRadiance(point: Point, u1: Float, u2: Float) uses to sample points (pbrt 14.6.1)
  def pdf(point: Point, wi: Vector): Float = shape.pdf(worldToLight * point, worldToLight * wi)

  // Total emitted power of this light source onto the scene
  def totalPower(scene: Scene): Spectrum = emitted * (shape.surfaceArea * π)

  // Emitted radiance in the given direction from the given point on the surface of the area light with the given normal
  def emittedRadiance(point: Point, normal: Normal, direction: Vector): Spectrum = if (normal * direction > 0.0f) emitted else Spectrum.Black

  override def toString = "DiffuseAreaLightSource(shape=%s, lightToWorld=%s, emitted=%s, numberOfSamples=%d)" format
    (shape, lightToWorld, emitted, numberOfSamples)
}
