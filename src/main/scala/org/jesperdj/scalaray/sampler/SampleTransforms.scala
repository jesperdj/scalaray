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
package org.jesperdj.scalaray.sampler

import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// Transforming samples between distributions (pbrt 14.4, 14.5)
object SampleTransforms {
	// Sample a point on the unit hemisphere by uniform mapping (pbrt 14.5.1)
	def uniformSampleHemisphere(u1: Float, u2: Float): Point = {
		val z = u1; val r = math.sqrt(math.max(0.0f, 1.0f - z * z)).toFloat; val phi = 2.0f * π * u2
		new Point(r * math.cos(phi).toFloat, r * math.sin(phi).toFloat, z)
	}

	// Probability distribution function for uniform sampling on the unit hemisphere (pbrt 14.5.1)
	val uniformHemispherePDF: Float = 1.0f / (2.0f * π)

	// Sample a point on the unit sphere by uniform mapping (pbrt 14.5.1)
	def uniformSampleSphere(u1: Float, u2: Float): Point = {
		val z = 1.0f - 2.0f * u1; val r = math.sqrt(math.max(0.0f, 1.0f - z * z)).toFloat; val phi = 2.0f * π * u2
		new Point(r * math.cos(phi).toFloat, r * math.sin(phi).toFloat, z)
	}

	// Probability distribution function for uniform sampling on the unit sphere (pbrt 14.5.1)
	val uniformSpherePDF: Float = 1.0f / (4.0f * π)

	// Sample a point on a disk by uniform mapping (pbrt 14.5.2)
	def uniformSampleDisk(u1: Float, u2: Float): (Float, Float) = {
		val r = math.sqrt(u1).toFloat; val theta = 2.0f * π * u2
		(r * math.cos(theta).toFloat, r * math.sin(theta).toFloat)
	}

	// Sample a point on a disk using Shirley's concentric mapping method (pbrt 14.5.2)
	def concentricSampleDisk(u1: Float, u2: Float): (Float, Float) = {
		val sx = 2.0f * u1 - 1.0f
		val sy = 2.0f * u2 - 1.0f

		// Handle degeneracy at the origin
		if (sx == 0.0f && sy == 0.0f) (0.0f, 0.0f) else {
			var r = 0.0f
			var theta = 0.0f

			if (sx >= -sy) {
				if (sx > sy) {
					// Handle first region of disk
					r = sx; theta = if (sy > 0.0f) sy / r else (8.0f + sy / r)
				}
				else {
					// Handle second region of disk
					r = sy; theta = 2.0f - sx / r
				}
			}
			else {
				if (sx <= sy) {
					// Handle third region of disk
					r = -sx; theta = 4.0f - sy / r
				}
				else {
					// Handle fourth region of disk
					r = -sy; theta = 6.0f + sx / r
				}
			}

			theta *= π / 4.0f

			(r * math.cos(theta).toFloat, r * math.sin(theta).toFloat)
		}
	}

	// Sample a point on the unit hemisphere using Malley's cosine mapping method (pbrt 14.5.3)
	def cosineSampleHemisphere(u1: Float, u2: Float): Point = {
		val (x, y) = concentricSampleDisk(u1, u2)
		new Point(x, y, math.sqrt(math.max(0.0f, 1.0f - x * x - y * y)).toFloat)
	}

	// Sample a point on a triangle by uniform mapping (pbrt 14.5.4), returns barycentric coordinates
	def uniformSampleTriangle(u1: Float, u2: Float): (Float, Float) = {
		val su1 = math.sqrt(u1).toFloat
		(1.0f - su1, u2 * su1)
	}
}
