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
import org.jesperdj.scalaray.renderer.Renderer
import org.jesperdj.scalaray.sampler.{ Sample, SampleSpec, SampleSpec1D, SampleSpec2D }
import org.jesperdj.scalaray.scene.{ Intersection, Scene }
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

// TODO: This needs to be refactored, can be substantially simplified now that the light source interface has been refactored.
// Has been patched to work with the new light source interface, but needs to be cleaned up thoroughly.

// Identifiers of sample patterns for an area light source
private final class SampleIDs (val lightSampleID: Int, val bsdfSampleID: Int, val bsdfComponentSampleID: Int) {
	override def toString = "SampleIDs(lightSampleID=%d, bsdfSampleID=%d, bsdfComponentSampleID=%d)" format (lightSampleID, bsdfSampleID, bsdfComponentSampleID)
}

// Direct lighting surface integrator (pbrt 15.1)
final class DirectLightingSurfaceIntegrator private (
	scene: Scene, val sampleSpecs: Traversable[SampleSpec],
	deltaLights: Traversable[LightSource], areaLights: Traversable[(LightSource, SampleIDs)]) extends SurfaceIntegrator {

	// Compute the incident radiance along the given ray
	def radiance(renderer: Renderer, ray: RayDifferential, intersection: Intersection, sample: Sample): Spectrum = {
		val dg = intersection.dg
		val wo = -ray.direction.normalize

		// Get emitted radiance if intersection point is on an area light source
		val emitted = intersection.emittedRadiance(wo)

		// TODO: This does not work if the light source is on the wrong side of the surface. Solve the self-intersection problem differently.
		// Point for shadow ray calculations just above surface to avoid self-intersection
		val shadowPoint = dg.point + dg.normal * 1e-6f

		// Get radiance of direct light from light sources on the intersection point
		val direct = uniformSampleAllLights(shadowPoint, dg.normal, wo, intersection.bsdf, sample)

		// TODO: trace rays for specular reflection and refraction (make recursion stop at a depth limit)

		emitted + direct
	}

	// Sample direct light from all light sources on the intersection point (pbrt 16.1)
	private def uniformSampleAllLights(point: Point, normal: Normal, wo: Vector, bsdf: BSDF, sample: Sample): Spectrum = {
		// Accumulate contributions of delta light sources
		val deltaLightRadiance = (Spectrum.Black /: deltaLights) { (accu, deltaLight) =>
			accu + estimateDirect(deltaLight, point, normal, wo, bsdf)
		}

		// Accumulate contributions of area light sources
		val areaLightRadiance = (Spectrum.Black /: areaLights) { case (accu, (areaLight, sampleIDs)) =>
			accu + estimateDirect(areaLight, point, normal, wo, bsdf,
								  sample.samples2D(sampleIDs.lightSampleID),
								  sample.samples2D(sampleIDs.bsdfSampleID),
								  sample.samples1D(sampleIDs.bsdfComponentSampleID))
		}

		deltaLightRadiance + areaLightRadiance
	}

	// TODO: Take transmittance along rays between light source and intersection point into account

	// Compute direct light from a delta light source on the intersection point
	private def estimateDirect(deltaLight: LightSource, point: Point, normal: Normal, wo: Vector, bsdf: BSDF): Spectrum = {
		val (radiance, ray, _) = deltaLight.sampleRadiance(point, 0.0f, 0.0f)
		if (radiance.isBlack) return Spectrum.Black

		val wi = -ray.direction.normalize

		// Evaluate BSDF
		val reflectance = bsdf(wo, wi)
		if (reflectance.isBlack) return Spectrum.Black

		// Trace shadow ray
		if (scene.checkIntersect(ray)) return Spectrum.Black

		radiance * reflectance * (wi * normal).abs
	}

	// Sample direct light from an area light source on the intersection point
	private def estimateDirect(areaLight: LightSource, point: Point, normal: Normal, wo: Vector, bsdf: BSDF,
							   lightSamples: IndexedSeq[FloatPair],
							   bsdfSamples: IndexedSeq[FloatPair], bsdfComponentSamples: IndexedSeq[Float]): Spectrum = {
		// Sample light source
		var lightContrib = Spectrum.Black
		for (i <- 0 until lightSamples.size) {
			val ls = lightSamples(i)

			val (radiance, ray, lightPdf) = areaLight.sampleRadiance(point, ls._1, ls._2)
			if (lightPdf > 0.0f && !radiance.isBlack) {
				val wi = -ray.direction.normalize

				// Evaluate BSDF; trace shadow ray
				val reflectance = bsdf(wo, wi)
				if (!reflectance.isBlack && !scene.checkIntersect(ray)) {
					// Add weighed contribution for this sample to total
					lightContrib +*= (radiance * reflectance, (wi * normal).abs * powerHeuristic(1, lightPdf, 1, bsdf.pdf(wo, wi)) / lightPdf)
				}
			}
		}

		// Sample BSDF
		var bsdfContrib = Spectrum.Black
		for (i <- 0 until bsdfSamples.size) {
			val bss = bsdfSamples(i); val bcs = bsdfComponentSamples(i)

			val (reflectance, wi, bsdfPdf, _) = bsdf.sample(wo, bss._1, bss._2, bcs)
			if (bsdfPdf > 0.0f && !reflectance.isBlack) {
				val lightPdf = areaLight.pdf(point, wi)
				if (lightPdf > 0.0f) {
					// Evaluate radiance from area light source
					val radiance = scene.intersect(Ray(point, wi)) match {
						case Some(Intersection(dg, prim, _)) if (prim.areaLightSource.isDefined && prim.areaLightSource.get == areaLight) =>
							// Ray intersects with area light source and point isn't shadowed
							areaLight.asInstanceOf[AreaLightSource].emittedRadiance(dg.point, dg.normal, -wi)

						case None => // No intersection
							// TODO: In pbrt wordt hier light->Le(ray) genomen, waarom? Waarschijnlijk voor infinite area light source.
							Spectrum.Black

						case _ => // Point is shadowed
							Spectrum.Black
					}

					if (!radiance.isBlack) {
						// Add weighed contribution for this sample to total
						bsdfContrib +*= (radiance * reflectance, (wi * normal).abs * powerHeuristic(1, bsdfPdf, 1, lightPdf) / bsdfPdf)
					}
				}
			}
		}

		lightContrib / lightSamples.size + bsdfContrib / bsdfSamples.size
	}

	// Balance heuristic weighing function for multiple importance sampling (pbrt 14.4.1)
	private def balanceHeuristic(nf: Int, fPdf: Float, ng: Int, gPdf: Float): Float =
		(nf * fPdf) / (nf * fPdf + ng * gPdf)

	// Power heuristic weighing function for multiple importance sampling (pbrt 14.4.1)
	private def powerHeuristic(nf: Int, fPdf: Float, ng: Int, gPdf: Float): Float = {
		val f = nf * fPdf; val g = ng * gPdf
		(f * f) / (f * f + g * g)
	}

	override def toString = "DirectLightingSurfaceIntegrator(sampleSpecs=%s, deltaLights=%s, areaLights=%s)" format (sampleSpecs, deltaLights, areaLights)
}

object DirectLightingSurfaceIntegrator {
	def apply(scene: Scene): DirectLightingSurfaceIntegrator = {
		val sampleSpecs = ListBuffer[SampleSpec]()
		val deltaLights = ListBuffer[LightSource]()
		val areaLights = ListBuffer[(LightSource, SampleIDs)]()

		scene.lightSources foreach { lightSource =>
			if (lightSource.isDeltaLight) {
				deltaLights += lightSource
			}
			else {
				val lightSampleSpec = new SampleSpec2D(lightSource.numberOfSamples); sampleSpecs += lightSampleSpec
				val bsdfSampleSpec = new SampleSpec2D(lightSource.numberOfSamples); sampleSpecs += bsdfSampleSpec
				val bsdfComponentSampleSpec = new SampleSpec1D(lightSource.numberOfSamples); sampleSpecs += bsdfComponentSampleSpec
				areaLights += ((lightSource, new SampleIDs(lightSampleSpec.id, bsdfSampleSpec.id, bsdfComponentSampleSpec.id)))
			}
		}

		new DirectLightingSurfaceIntegrator(scene, sampleSpecs.toList, deltaLights.toList, areaLights.toList)
	}
}
