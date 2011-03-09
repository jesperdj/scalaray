/*
 * ScalaRay - Ray tracer based on pbrt (see http://pbrt.org) written in Scala
 * Copyright (C) 2009, 2010, 2011  Jesper de Jong
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
import org.jesperdj.scalaray.sampler._
import org.jesperdj.scalaray.scene._
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.vecmath.RayDifferential

// TODO: Rewrite this using actors (see mandelactors)
// TODO: Ugly that we have to pass the number of samples per pixel to this thing

// Sampler renderer (pbrt 1.3.3)
final class SamplerRenderer (scene: Scene, sampler: Sampler, samplesPerPixel: Int, camera: Camera, pixelBuffer: PixelBuffer,
                             surfaceIntegrator: SurfaceIntegrator, volumeIntegrator: VolumeIntegrator) extends Renderer {
  // Render the scene
  def render(): Unit = {
    import java.util.concurrent._
    import java.util.concurrent.atomic.AtomicInteger

    val runningTasks = new AtomicInteger

    val scale = 1.0 / math.sqrt(samplesPerPixel)

    final class Task (batch: SampleBatch) extends Runnable {
      def run() {
        batch.samples foreach { sample => pixelBuffer += (sample, radiance(camera.generateRayDifferential(sample, scale), sample)) }
        runningTasks.decrementAndGet
      }
    }

    val processors = Runtime.getRuntime().availableProcessors()
    println("Number of processors: " + processors)

    // Create executor service and submit tasks
    val executorService = Executors.newFixedThreadPool(processors)
    sampler.batches.foreach { batch => runningTasks.incrementAndGet; executorService.submit(new Task(batch)) }

    val numTasks = sampler.batches.size

    // Wait until all tasks have finished
    executorService.shutdown()
    while (!executorService.isTerminated()) {
      executorService.awaitTermination(5, TimeUnit.SECONDS)
      println("%d/%d tasks done (%d%%)" format
          (numTasks - runningTasks.intValue, numTasks, 100 - ((100 * runningTasks.get) / numTasks)))
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
