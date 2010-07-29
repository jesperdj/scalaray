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
package org.jesperdj.scalaray.renderer

import org.jesperdj.scalaray.camera.Camera
import org.jesperdj.scalaray.integrator.{ SurfaceIntegrator, VolumeIntegrator }
import org.jesperdj.scalaray.raster.Raster
import org.jesperdj.scalaray.sampler.{ Sample, Sampler }
import org.jesperdj.scalaray.scene._
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.RayDifferential

// Sampler renderer (pbrt 1.3.3)
final class SamplerRenderer (scene: Scene, sampler: Sampler, camera: Camera, raster: Raster,
							 surfaceIntegrator: SurfaceIntegrator, volumeIntegrator: VolumeIntegrator) extends Renderer {
	// Render the scene
	def render(): Unit = {
		import java.util.concurrent._
		import java.util.concurrent.atomic.AtomicInteger

		var runningTasks = new AtomicInteger

		final class Task (sampler: Sampler) extends Runnable {
			def run() {
				val scale: Float = 1.0f / math.sqrt(sampler.samplesPerPixel).toFloat
				sampler.samples foreach { sample => raster.addSample(sample, radiance(camera.generateRayDifferential(sample, scale), sample)) }
				runningTasks.decrementAndGet
			}
		}

		val processors = Runtime.getRuntime().availableProcessors()
		println("Number of processors: " + processors)

		// Create executor service and submit tasks
		val executorService = Executors.newFixedThreadPool(processors)
		val samplers = sampler.split(math.max(32 * processors, raster.rectangle.width * raster.rectangle.height / 256))
		samplers.foreach { s => runningTasks.incrementAndGet; executorService.submit(new Task(s)) }

		val numSamplers = samplers.size

		// Wait until all tasks have finished
		executorService.shutdown()
		while (!executorService.isTerminated()) {
			executorService.awaitTermination(10, TimeUnit.SECONDS)
			println("%d/%d tasks done (%d%%)" format
					(numSamplers - runningTasks.intValue, numSamplers, 100 - ((100 * runningTasks.get) / numSamplers)))
		}

	}

	// Compute the incident radiance along the given ray (pbrt 1.3.4)
	def radiance(ray: RayDifferential, sample: Sample): Spectrum = {
		val li = scene intersect ray match {
			// If the ray intersects geometry in the scene, get the reflected radiance from the surface integrator
			case Some(intersection) => surfaceIntegrator.radiance(this, ray, intersection, sample)

			// TODO: If the ray does not intersect any geometry, accumulate the contributions of infinite area light sources along the ray
			case _ => Spectrum.Black
		}

		val (lvi, t) = volumeIntegrator.radiance(this, ray, sample)

		t * li + lvi
	}

	// Compute the fraction of light that is attenuated by volumetric scattering along the ray (pbrt 1.3.4)
	def transmittance(ray: RayDifferential, sample: Sample): Spectrum = volumeIntegrator.transmittance(this, ray, sample)

	override def toString = "SamplerRenderer"
}
