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

import scala.collection.Iterator
import scala.collection.immutable.Map

// Camera sample
sealed class CameraSample (val imageX: Double, val imageY: Double, val lensU: Double, val lensV: Double, val time: Double)

// Sample, a camera sample with 1D and 2D sample patterns for integrators
final class Sample (imageX: Double, imageY: Double, lensU: Double, lensV: Double, time: Double,
                    val samplePatterns1D: Map[Int, SamplePattern1D], val samplePatterns2D: Map[Int, SamplePattern2D])
  extends CameraSample(imageX, imageY, lensU, lensV, time)

// A batch of samples
trait SampleBatch extends Iterator[Sample]

// Sampler (pbrt 7.2)
trait Sampler extends Iterator[SampleBatch]
