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
import scala.collection.mutable.{ ListBuffer, Map => MutableMap }

import org.jesperdj.scalaray.lightsource._
import org.jesperdj.scalaray.reflection.BSDF
import org.jesperdj.scalaray.sampler._
import org.jesperdj.scalaray.scene.Scene
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath._

// Direct lighting surface integrator (pbrt 16.1)
final class DirectLightingSurfaceIntegrator private (
	scene: Scene, deltaLights: Traversable[DeltaLightSource], areaLights: Map[Int, AreaLightSource],
	val sampleSpecs: Traversable[SampleSpec]) extends SurfaceIntegrator {

	// Radiance along the ray (pbrt 16)
	def radiance(ray: Ray, sample: Sample): Spectrum = scene intersect ray match {
		case Some(intersection) =>
			val point = intersection.differentialGeometry.point
			val normal = intersection.differentialGeometry.normal
			val wo = -ray.direction
			val bsdf = intersection.bsdf

			// TODO: this only works if the light is on the "outside" of the surface, for the "inside", it should be point - normal * 1e-6
			// TODO: Solve this differently; check explicitly for self intersection
			// Point for shadow ray calculations just above surface to avoid self-intersection
			val shadowPoint = point + normal * 1e-6

			intersection.emittedRadiance(wo) +
			uniformSampleAllLights(shadowPoint, normal, wo, bsdf, sample) +
			sampleSpecularReflection(shadowPoint, normal, wo, bsdf, sample) +
			sampleSpecularTransmission(shadowPoint, normal, wo, bsdf, sample)

		case None => // No intersection
			Spectrum.Black
	}

	// Compute incident radiance from all light sources at the point in the outgoing direction wo
	private def uniformSampleAllLights(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum = {
		// TODO: The BSDF is currently ignored, implement sampling the BSDF

		def deltaLightRadiance(accumulator: Spectrum, lightSource: DeltaLightSource): Spectrum = {
			val (lr, ray) = lightSource.incidentRadiance(point)
			if (!lr.isBlack && scene.intersect(ray).isEmpty) accumulator +* (lr, math.abs(ray.direction.normalize * normal)) else accumulator
		}

		def areaLightRadiance(accumulator: Spectrum, entry: (Int, AreaLightSource)): Spectrum = {
			val id = entry._1; val lightSource = entry._2

			// Sample pattern for this light source
			val lightSamples = sample.samples2D(id)

			// Accumulate contributions of light samples
			var count = 0
			val sampleRadiance = lightSamples.foldLeft(Spectrum.Black) { case (radiance, (ls1, ls2)) =>
				count += 1
				val (lr, ray, pdf) = lightSource.incidentRadiance(point, ls1, ls2)
				if (pdf > 0.0 && !lr.isBlack && scene.intersect(ray).isEmpty) radiance +* (lr, math.abs(ray.direction.normalize * normal) / pdf) else radiance
			}

			// Add average of samples to the result
			accumulator +* (sampleRadiance, 1.0 / count)
		}

		// Compute total of contributions from delta light sources and area light sources
		deltaLights.foldLeft(Spectrum.Black) (deltaLightRadiance(_, _)) + areaLights.foldLeft(Spectrum.Black) (areaLightRadiance(_, _))
	}

	// TODO
	private def sampleSpecularReflection(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum =
		Spectrum.Black // TODO

	// TODO
	private def sampleSpecularTransmission(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum =
		Spectrum.Black // TODO

	override def toString = "DirectLightingSurfaceIntegrator"
}

object DirectLightingSurfaceIntegrator {
	def apply(scene: Scene): DirectLightingSurfaceIntegrator = {
		val deltaLights = ListBuffer[DeltaLightSource]()
		val areaLights = MutableMap[Int, AreaLightSource]()
		val sampleSpecs = ListBuffer[SampleSpec]()

		def process(lightSource: LightSource): Unit = lightSource match {
			case ls: DeltaLightSource => deltaLights += ls
			case ls: AreaLightSource => val ss = new SampleSpec2D(ls.numberOfSamplesX, ls.numberOfSamplesY); sampleSpecs += ss; areaLights += ss.id -> ls
		}

		scene.lightSources foreach (process(_))

		new DirectLightingSurfaceIntegrator(scene, deltaLights.toList, areaLights.toMap, sampleSpecs.toList)
	}
}
