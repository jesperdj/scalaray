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
package org.jesperdj.scalaray

import org.jesperdj.scalaray.vecmath._

package object shape {
	// Implicit conversion for transforming a bounding box (pbrt 2.8.5)
	implicit def implicitTransformBoundingBox(t: Transform) = new ImplicitTransformBoundingBox(t)

	final class ImplicitTransformBoundingBox (t: Transform) {
		def *(bb: BoundingBox) = new BoundingBox(bb.corners map (t * _ ))
	}

	// Implicit conversion for transforming a DifferentialGeometry
	implicit def implicitTransformDifferentialGeometry(t: Transform) = new ImplicitTransformDifferentialGeometry(t)

	final class ImplicitTransformDifferentialGeometry (t: Transform) {
		def *(dg: DifferentialGeometry) = new DifferentialGeometry {
			// Point on the surface
			lazy val point = t * dg.point

			// Surface normal at the point
			override lazy val normal = (t * dg.normal).normalize

			// Surface parameter coordinates
			lazy val u = dg.u
			lazy val v = dg.v

			// Partial derivatives of the surface position with respect to the (u, v) coordinates
			lazy val dpdu = t * dg.dpdu
			lazy val dpdv = t * dg.dpdv

			// Partial derivatives of the surface normal with respect to the (u, v) coordinates
			lazy val dndu = t * dg.dndu
			lazy val dndv = t * dg.dndv

			// Shape which defines the surface
			val shape = dg.shape
		}
	}
}
