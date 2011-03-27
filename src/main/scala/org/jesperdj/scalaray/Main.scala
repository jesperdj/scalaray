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
package org.jesperdj.scalaray

import java.io.File
import javax.imageio.ImageIO

import org.jesperdj.scalaray.camera._
import org.jesperdj.scalaray.filter._
import org.jesperdj.scalaray.integrator._
import org.jesperdj.scalaray.raster._
import org.jesperdj.scalaray.renderer._
import org.jesperdj.scalaray.sampler._
import org.jesperdj.scalaray.common._
import org.jesperdj.scalaray.vecmath._

class Timer (val name: String) {
  private var totalTime: Long = 0L
  private var startTime: Long = 0L

  def start() { startTime = System.nanoTime }
  def stop() { totalTime += System.nanoTime - startTime }

  def time[T](block: => T): T = { start; val result: T = block; stop; result }

  def total = totalTime

  override def toString = "%s: %g seconds" format (name, totalTime / 1e9)
}

object Main {
  def main(args: Array[String]) {
    println("ScalaRay - Ray tracer based on pbrt (see http://pbrt.org) written in Scala")
    println("Copyright (C) 2009, 2010, 2011  Jesper de Jong")

//    println("Press Enter to start")
//    new java.util.Scanner(System.in).nextLine()

    println()
    println("Setup...")
    val scene = Scene01.createScene()

    val rect = new Rectangle(800, 600)

    val samplePatternSpecs = new ListBuilder[SamplePatternSpec]

    val surfaceIntegratorBuilder = new DirectLightingSurfaceIntegratorBuilder().withSamplePatternSpecs(samplePatternSpecs)
    val volumeIntegratorBuilder = VacuumVolumeIntegratorBuilder
    val integrator = new Integrator(scene, surfaceIntegratorBuilder, volumeIntegratorBuilder)

    val sampler: Sampler = new StratifiedSampler(rect, 16384, 2, 2, true, samplePatternSpecs.build())
    val filter: Filter = new BoxFilter

    val camera: Camera = new PerspectiveCamera(Transform.translate(0.0, 0.75, 0.0), π / 4.0, rect.width, rect.height)

    val renderer: Renderer = new SamplerRenderer(sampler, filter, camera, integrator)

//    println("- Surface integrator: " + surfaceIntegrator)
//    println("- Volume integrator: " + volumeIntegrator)
    println("- Sampler: " + sampler)
    println("- Filter: " + filter)
    println("- Renderer: " + renderer)
    println("- Camera: " + camera)

    println()
    println("Rendering...")
    val timer = new Timer("Total rendering time")
    val pixelBuffer = timer.time { renderer.render() }
    println(timer.toString)

    ImageIO.write(pixelBuffer.toImage, "png", new File("output.png"))

    println()
    println("Finished")
  }
}
