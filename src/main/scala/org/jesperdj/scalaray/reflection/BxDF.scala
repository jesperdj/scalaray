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

import scala.collection.immutable.Traversable

import org.jesperdj.scalaray.sampler.SampleTransforms
import org.jesperdj.scalaray.spectrum._
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// Bidirectional Reflectance or Transmittance Distribution Function (pbrt 8.1)
abstract class BxDF {
	// BxDF type
	val bxdfType: BxDFType

	// Check if the type of this BxDF matches the given flags
	def matchesType(flags: BxDFType): Boolean = bxdfType.matches(flags)

	// Evaluate the BxDF for the given pair of directions
	def apply(wo: Vector, wi: Vector): Spectrum

	// Sample the BxDF for the given outgoing direction; returns reflectance or transmittance, incoming direction and value of the pdf (pbrt 14.5)
	def sample(wo: Vector, u1: Float, u2: Float): (Spectrum, Vector, Float) = {
		val wi = { val p = SampleTransforms.cosineSampleHemisphere(u1, u2); if (wo.z >= 0.0f) Vector(p) else Vector(p.x, p.y, -p.z) }
		(apply(wo, wi), wi, pdf(wo, wi))
	}

	// Get the value of the probability distribition function that matches the sampling method of sample(Vector, Float, Float) (pbrt 14.5)
	def pdf(wo: Vector, wi: Vector): Float = if (wo.z * wi.z > 0.0f) wi.z.abs / π else 0.0f

	// Compute hemispherical-directional reflectance (pbrt 8.1.1, 14.5.5)
	def rho(wo: Vector, samples: Traversable[(Double, Double)]): Spectrum =
		throw new UnsupportedOperationException("Not yet implemented") // TODO

	// Compute hemispherical-hemispherical reflectance (pbrt 8.1.1, 14.5.5)
	def rho(samples1: Traversable[(Double, Double)], samples2: Traversable[(Double, Double)]): Spectrum =
		throw new UnsupportedOperationException("Not yet implemented") // TODO
}
