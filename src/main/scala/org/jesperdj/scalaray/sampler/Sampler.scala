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
package org.jesperdj.scalaray.sampler

import org.jesperdj.scalaray.raster.Rectangle

import scala.collection.Iterator
import scala.collection.immutable.Map

// NOTE: In pbrt, Sample is a subclass of CameraSample, which is, in my opinion, a misuse of inheritance.
// ScalaRay uses composition instead of inheritance here.

// Camera sample
final case class CameraSample (imageX: Double, imageY: Double, lensU: Double, lensV: Double, time: Double)

// Sample, consists of a camera sample and 1D and 2D sample patterns for integrators
final case class Sample (cameraSample: CameraSample, samplePatterns1D: Map[Int, SamplePattern1D], samplePatterns2D: Map[Int, SamplePattern2D])

// A batch of samples
trait SampleBatch extends Iterator[Sample]

// Sampler (pbrt 7.2)
trait Sampler extends Iterator[SampleBatch] {
  // Rectangle over which this sampler generates samples
  val rectangle: Rectangle

  // Number of samples per pixel that this sampler generates
  val samplesPerPixel: Int

  // Number of batches that this sampler generates
  val numberOfBatches: Int
}
