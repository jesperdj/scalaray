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
package org.jesperdj.scalaray.scene

import org.jesperdj.scalaray.reflection.BSDF
import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath._

// Intersection with a primitive (pbrt 4.1)
final class Intersection (val dg: DifferentialGeometry, val primitive: GeometricPrimitive, val objectToWorld: Transform) {
  // Get the BSDF at the intersection point
  def bsdf: BSDF = primitive.bsdf(dg, objectToWorld)

  // Get the emitted radiance from the intersection point if the intersection is on an area light (pbrt 12.4)
  def emittedRadiance(direction: Vector): Spectrum = primitive.areaLightSource match {
    case Some(areaLightSource) => areaLightSource.emittedRadiance(dg.point, dg.normal, direction)
    case None => Spectrum.Black
  }

  override def toString = "Intersection(differentialGeometry=%s, primitive=%s, objectToWorld=%s)" format (dg, primitive, objectToWorld)
}

object Intersection {
  // Extractor method
  def unapply(intersection: Intersection): Option[(DifferentialGeometry, GeometricPrimitive, Transform)] =
    Some(intersection.dg, intersection.primitive, intersection.objectToWorld)
}
