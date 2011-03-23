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
import org.jesperdj.scalaray.filter.Filter
import org.jesperdj.scalaray.integrator.Integrator
import org.jesperdj.scalaray.sampler.{ SampleBatch, Sampler }

// Sampler renderer (pbrt 1.3.3)
final class SamplerRenderer (sampler: Sampler, filter: Filter, camera: Camera, integrator: Integrator) extends Renderer {
  def render(): PixelBuffer = {
    import scala.actors.Actor._
    import java.util.concurrent.{ CountDownLatch, TimeUnit }

    val pixelBuffer = new PixelBuffer(sampler.rectangle, filter)

    val latch = new CountDownLatch(sampler.numberOfBatches)

    for (batch <- sampler) {
      // Create an actor to handle this batch
      val computeActor = actor {
        react {
          case batch: SampleBatch =>
            for (sample <- batch) {
              val ray = camera.generateRay(sample.cameraSample)
              val spectrum = integrator.radiance(ray, sample)
              pixelBuffer += (sample, spectrum)
            }

            latch.countDown
        }
      }

      // Send the batch to the actor
      computeActor ! batch
    }

    // Wait until all the actors have finished
    while (!latch.await(5, TimeUnit.SECONDS)) {
      val count = sampler.numberOfBatches - latch.getCount()
      println("%d/%d batches processed (%d%%)" format (count, sampler.numberOfBatches, (count * 100) / sampler.numberOfBatches))
    }

    pixelBuffer
  }

  override def toString = "SamplerRenderer"
}
