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
package org.jesperdj.scalaray.reflection

import scala.collection.immutable.IndexedSeq

import org.jesperdj.scalaray.shape.DifferentialGeometry
import org.jesperdj.scalaray.spectrum._
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// TODO: Not yet implemented

// Bidirectional Scattering Distribution Function
final class BSDF (dg: DifferentialGeometry, bxdfs: IndexedSeq[BxDF]) {
	private val sn = dg.dpdu.normalize
	private val tn = dg.normal ** sn
	private val nn = dg.normal

	private def worldToLocal(v: Vector) = Vector(v * sn, v * tn, v * nn)

	private def localToWorld(v: Vector) = Vector(
		sn.x * v.x + tn.x * v.y + nn.x * v.z,
		sn.y * v.x + tn.y * v.y + nn.y * v.z,
		sn.z * v.x + tn.z * v.y + nn.z * v.z)

	// TODO: Description
	def apply(wo: Vector, wi: Vector): Spectrum =
		Spectrum.Unit // TODO

	// TODO: Description. Returns reflectance, wi and pdf
	def sample(wo: Vector, u1: Float, u2: Float, u3: Float): (Spectrum, Vector, Float) =
		(Spectrum.Unit, Vector(-wo.x, -wo.y, wo.z), 1.0f) // TODO

	// TODO: Description
	def pdf(wo: Vector, wi: Vector): Float =
		if (wo.z * wi.z > 0.0f) wi.z.abs / π else 0.0f // TODO
}
