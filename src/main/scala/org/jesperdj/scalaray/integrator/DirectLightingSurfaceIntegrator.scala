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
package org.jesperdj.scalaray.integrator

import scala.collection.immutable.{ Map, Traversable }
import scala.collection.mutable.ListBuffer

import org.jesperdj.scalaray.lightsource._
import org.jesperdj.scalaray.sampler._
import org.jesperdj.scalaray.scene.Scene
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath._

// Direct lighting surface integrator (pbrt 16.1)
final class DirectLightingSurfaceIntegrator (scene: Scene) extends SurfaceIntegrator {
	// Determine sample pattern specifications and map of area light sources
	private val (sampleSpecsInit: Traversable[SampleSpec], areaLightMap: Map[Int, AreaLightSource]) = {
		var ss: ListBuffer[SampleSpec] = ListBuffer()
		var lm: Map[Int, AreaLightSource] = Map()

		scene.areaLightSources foreach { areaLight =>
			val s = new SampleSpec2D(areaLight.numberOfSamplesX, areaLight.numberOfSamplesY)
			ss += s; lm += s.id -> areaLight
		}

		(Traversable() ++ ss, lm)
	}

	// Sample specifications for the sample patterns that this integrator needs
	val sampleSpecs: Traversable[SampleSpec] = sampleSpecsInit

	// Compute radiance along the ray (pbrt 16.1)
	def computeRadiance(ray: Ray, sample: Sample): Spectrum = {
		scene intersect ray match {
			case Some(intersection) =>
				val point = intersection.differentialGeometry.point
				val normal = intersection.differentialGeometry.normal
				val direction = -ray.direction

				// Point for shadow ray calculations just above surface to avoid self-intersection
				val shadowPoint = point + normal * 1e-6

				// Calculate contribution of light sources and add emitted radiance (if the intersection is with an area light)
				uniformSampleAllLights(sample, shadowPoint, normal, direction) + intersection.emittedRadiance(point, normal, direction)

				// TODO: Add contributions of rays for specular reflection and refraction

			case None => Spectrum.Black
		}
	}

	// TODO: Description (pbrt 16.1)
	private def uniformSampleAllLights(sample: Sample, point: Point, normal: Normal, wo: Vector): Spectrum = {
		// Compute contribution of delta light sources
		val deltaRadiance = scene.deltaLightSources.foldLeft(Spectrum.Black) { (spectrum, lightSource) =>
			spectrum + estimateDirect(lightSource, point, normal, wo)
		}

		// Compute contribution of area light sources
		val areaRadiance = areaLightMap.foldLeft(Spectrum.Black) { case (spectrum, (id, lightSource)) =>
			// Sample pattern for this light source
			val lightSamples = sample.samples2D(id)

			// Accumulate contributions of samples
			val r = lightSamples.foldLeft(Spectrum.Black) { case (sp, (ls1, ls2)) =>
				sp + estimateDirect(lightSource, ls1, ls2, point, normal, wo)
			}

			spectrum +* (r, 1.0 / lightSamples.size)
		}

		deltaRadiance + areaRadiance
	}

	// TODO: Description (pbrt 16.1.1)
	private def estimateDirect(lightSource: DeltaLightSource, point: Point, normal: Normal, wo: Vector): Spectrum = {
		val (radiance, ray) = lightSource.sampleRadiance(point)
		if (!radiance.isBlack && scene.intersect(ray).isEmpty) radiance * math.abs(ray.direction.normalize * normal) else Spectrum.Black
	}

	// TODO: Description (pbrt 16.1.1)
	private def estimateDirect(lightSource: AreaLightSource, ls1: Double, ls2: Double, point: Point, normal: Normal, wo: Vector) = {
		val (radiance, ray, pdf) = lightSource.sampleRadiance(point, ls1, ls2)
		if (pdf > 0.0 && !radiance.isBlack && scene.intersect(ray).isEmpty) {
			// Multiple importance sampling using power heuristic
			val weight = powerHeuristic(1, pdf, 1, 0.0)
			radiance * (math.abs(ray.direction.normalize * normal) * weight / pdf)
		}
		else
			Spectrum.Black
	}

    // Power heuristic for multiple importance sampling (pbrt 15.4.1)
    private def powerHeuristic(nf: Int, fpdf: Double, ng: Int, gpdf: Double): Double = {
        val f = nf * fpdf
        val g = ng * gpdf
        (f * f) / (f * f + g * g)
    }

	override def toString = "DirectLightingSurfaceIntegrator"
}
