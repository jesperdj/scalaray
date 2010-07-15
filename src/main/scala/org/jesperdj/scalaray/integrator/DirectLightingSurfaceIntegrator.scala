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

import scala.collection.immutable.Traversable
import scala.collection.mutable.ListBuffer

import org.jesperdj.scalaray.lightsource._
import org.jesperdj.scalaray.reflection.BSDF
import org.jesperdj.scalaray.sampler.{ Sample, SampleSpec, SampleSpec1D, SampleSpec2D }
import org.jesperdj.scalaray.scene.Scene
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath._

// An area light source and the identifiers of the sample patterns that are associated with it
private class AreaLightSampleIDs (val areaLightSource: AreaLightSource, val lightSampleID: Int, val bsdfSampleID: Int, val bsdfComponentSampleID: Int) {
	override def toString = "AreaLightSampleIDs(areaLightSource=%s, lightSampleID=%d, bsdfSampleID=%d, bsdfComponentSampleID=%d)" format
		(areaLightSource, lightSampleID, bsdfSampleID, bsdfComponentSampleID)
}

// Direct lighting surface integrator (pbrt 16.1)
final class DirectLightingSurfaceIntegrator private (
	scene: Scene, val sampleSpecs: Traversable[SampleSpec],
	deltaLights: Traversable[DeltaLightSource], areaLights: Traversable[AreaLightSampleIDs]) extends SurfaceIntegrator {

	// Radiance along the ray (pbrt 16)
	def radiance(ray: Ray, sample: Sample): Spectrum = scene intersect ray match {
		case Some(intersection) =>
			val dg = intersection.differentialGeometry
			val wo = -ray.direction.normalize

			// Get emitted radiance if intersection point is on an area light source
			val emitted = intersection.emittedRadiance(wo)

			// TODO: This does not work if the light source is on the wrong side of the surface. Solve the self-intersection problem differently.
			// Point for shadow ray calculations just above surface to avoid self-intersection
			val shadowPoint = dg.point + dg.normal * 1e-6

			// Get radiance of direct light from light sources on the intersection point
			val direct = uniformSampleAllLights(shadowPoint, dg.normal, wo, intersection.bsdf, sample)

			// TODO: trace rays for specular reflection and refraction (make recursion stop at a depth limit)

			emitted + direct

		case None => // No intersection
			Spectrum.Black
	}

	// Sample direct light from all light sources on the intersection point (pbrt 16.1)
	private def uniformSampleAllLights(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum = {
		// Accumulate contributions of delta light sources
		val deltaLightRadiance = (Spectrum.Black /: deltaLights) { (accu, deltaLight) =>
			accu + computeDirect(deltaLight, point, normal, wo, bsdf)
		}

		// Accumulate contributions of area light sources
		val areaLightRadiance = (Spectrum.Black /: areaLights) { (accu, areaLight) =>
			// Get sample patterns for this area light
			val lightSamples = sample.samples2D(areaLight.lightSampleID)
			val bsdfSamples = sample.samples2D(areaLight.bsdfSampleID)
			val bsdfComponentSamples = sample.samples1D(areaLight.bsdfComponentSampleID)

			// Estimate radiance for all samples
			var radiance = Spectrum.Black
			for (i <- 0 until lightSamples.size)
				radiance += estimateDirect(areaLight.areaLightSource, point, normal, wo, bsdf, lightSamples(i), bsdfSamples(i), bsdfComponentSamples(i))

			accu +* (radiance, 1.0 / lightSamples.size)
		}

		deltaLightRadiance + areaLightRadiance
	}

	// Compute direct light from a delta light source on the intersection point
	private def computeDirect(deltaLight: DeltaLightSource, point: Point, normal: Normal, wo: Vector, bsdf: BSDF): Spectrum = {
		val (radiance, ray) = deltaLight.incidentRadiance(point)
		if (radiance.isBlack) return Spectrum.Black

		val wi = -ray.direction.normalize

		// Evaluate BSDF
		val f = bsdf(wo, wi)
		if (f.isBlack) return Spectrum.Black

		// Trace shadow ray
		if (scene.intersect(ray).isDefined) return Spectrum.Black

		f * radiance * math.abs(wi * normal)
	}

	// Estimate direct light from an area light source on the intersection point (pbrt 16.1)
	private def estimateDirect(areaLight: AreaLightSource, point: Point, normal: Normal, wo: Vector, bsdf: BSDF,
							   lightSample: (Double, Double), bsdfSample: (Double, Double), bsdfComponentSample: Double): Spectrum = {
		val (radiance, ray, pdf) = areaLight.sampleRadiance(point, lightSample._1, lightSample._2)
		if (radiance.isBlack || pdf == 0.0) return Spectrum.Black

		// Trace shadow ray
		if (scene.intersect(ray).isDefined) return Spectrum.Black

		val wi = -ray.direction.normalize
		radiance * (math.abs(wi * normal) / pdf)

		// TODO: sample light source and bsdf using multiple importance sampling (pbrt 15.4.1)
	}

	override def toString = "DirectLightingSurfaceIntegrator(sampleSpecs=%s, deltaLights=%s, areaLights=%s)" format (sampleSpecs, deltaLights, areaLights)
}

object DirectLightingSurfaceIntegrator {
	def apply(scene: Scene): DirectLightingSurfaceIntegrator = {
		val sampleSpecs = ListBuffer[SampleSpec]()
		val deltaLights = ListBuffer[DeltaLightSource]()
		val areaLights = ListBuffer[AreaLightSampleIDs]()

		scene.lightSources foreach {
			_ match {
				case lightSource: DeltaLightSource =>
					deltaLights += lightSource

				case lightSource: AreaLightSource =>
					val lightSampleSpec = new SampleSpec2D(lightSource.numberOfSamples); sampleSpecs += lightSampleSpec
					val bsdfSampleSpec = new SampleSpec2D(lightSource.numberOfSamples); sampleSpecs += bsdfSampleSpec
					val bsdfComponentSampleSpec = new SampleSpec1D(lightSource.numberOfSamples); sampleSpecs += bsdfComponentSampleSpec
					areaLights += new AreaLightSampleIDs(lightSource, lightSampleSpec.id, bsdfSampleSpec.id, bsdfComponentSampleSpec.id)
			}
		}

		new DirectLightingSurfaceIntegrator(scene, sampleSpecs.toList, deltaLights.toList, areaLights.toList)
	}
}
