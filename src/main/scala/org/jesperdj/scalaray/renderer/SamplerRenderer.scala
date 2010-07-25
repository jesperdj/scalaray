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
import org.jesperdj.scalaray.scene.Scene
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.Ray

// Sampler renderer
final class SamplerRenderer (scene: Scene, sampler: Sampler, surfaceIntegrator: SurfaceIntegrator, volumeIntegrator: VolumeIntegrator) extends Renderer {

	private var runningTasks = new java.util.concurrent.atomic.AtomicInteger

	private final class SamplerRendererTask (camera: Camera, raster: Raster, sampler: Sampler) extends Runnable {
		def run() {
			for (sample <- sampler.samples) {
				val (rad, _) = radiance(camera.generateRay(sample), sample)
				raster.addSample(sample, rad)
			}
			runningTasks.decrementAndGet
		}
	}

	// Render a frame using the given camera and store the result in the given raster
	def render(camera: Camera, raster: Raster): Unit = {
		import java.util.concurrent._

		// Determine number of tasks to create
		val processors = Runtime.getRuntime().availableProcessors()
		val numTasks = math.max(32 * processors, raster.rectangle.width * raster.rectangle.height / 256)

		// Create executor service and tasks
		val executorService = Executors.newFixedThreadPool(processors)
		val samplers = sampler.split(numTasks)
		samplers.foreach { s => runningTasks.incrementAndGet; executorService.submit(new SamplerRendererTask(camera, raster, s)) }

		val numSamplers = samplers.size

		// Wait until all tasks have finished
		executorService.shutdown()
		while (!executorService.isTerminated()) {
			executorService.awaitTermination(10, TimeUnit.SECONDS)
			println("%d/%d tasks done (%d%%)" format
					(numSamplers - runningTasks.intValue, numSamplers, 100 - (100.0f * runningTasks.floatValue / numSamplers).toInt))
		}
	}

	// Compute the radiance along the given ray; returns radiance and transmittance
	def radiance(ray: Ray, sample: Sample): (Spectrum, Spectrum) = {
		val (rad, trn) = volumeIntegrator.radiance(ray, sample)
		(trn * surfaceIntegrator.radiance(ray, sample) + rad, trn)
	}

	// Compute the transmittance along the given ray
	def transmittance(ray: Ray, sample: Sample): Spectrum = volumeIntegrator.transmittance(ray, sample)

	override def toString = "SamplerRenderer"
}
