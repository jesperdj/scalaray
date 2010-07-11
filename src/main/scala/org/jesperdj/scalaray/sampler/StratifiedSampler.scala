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
package org.jesperdj.scalaray.sampler

import scala.collection.immutable.{ IndexedSeq, Traversable }
import scala.collection.mutable.{ Map => MutableMap }

import org.jesperdj.scalaray._
import org.jesperdj.scalaray.raster.Rectangle

// Stratified sampler (pbrt 7.3)
final class StratifiedSampler (rectangle: Rectangle, samplesPerPixelX: Int, samplesPerPixelY: Int, sampleSpecs: Traversable[SampleSpec])
	extends PixelSampler(rectangle, samplesPerPixelX, samplesPerPixelY) {

	// Generate samples for one pixel
	protected def generateSamples(x: Int, y: Int): IndexedSeq[Sample] = {
		// Generate image, lens and time samples; shuffle lens and time samples
		val imageSamples = StratifiedSampler.generateSamples2D(samplesPerPixelX, samplesPerPixelY)
		val lensSamples = shuffle(StratifiedSampler.generateSamples2D(samplesPerPixelX, samplesPerPixelY))
		val timeSamples = shuffle(StratifiedSampler.generateSamples1D(samplesPerPixel))

		for (i <- 0 until samplesPerPixel) yield {
			// Generate 1D and 2D sample patterns for the current sample
			val samples1D = MutableMap[Int, Traversable[Double]]()
			val samples2D = MutableMap[Int, Traversable[(Double, Double)]]()

			sampleSpecs foreach {
				_ match {
					case spec: SampleSpec1D => samples1D += spec.id -> (IndexedSeq() ++ shuffle(StratifiedSampler.generateSamples1D(spec.count)))
					case spec: SampleSpec2D => samples2D += spec.id -> (IndexedSeq() ++ shuffle(StratifiedSampler.generateSamples2D(spec.countX, spec.countY)))
				}
			}

			val (ix, iy) = imageSamples(i)
			val (lu, lv) = lensSamples(i)

			// Create Sample object, shift image samples to pixel position
			new Sample(ix + x, iy + y, lu, lv, timeSamples(i), samples1D.toMap, samples2D.toMap)
		}
	}

	override def toString = "StratifiedSampler(rectangle=%s, samplesPerPixelX=%d, samplesPerPixelY=%d, sampleSpecs=%s)" format
		(rectangle, samplesPerPixelX, samplesPerPixelY, sampleSpecs)
}

object StratifiedSampler {
	// NOTE: scala.util.Random is thread-safe because it uses java.util.Random which is thread-safe
	private val random = new scala.util.Random

	// Generate a set of stratified 1D samples
	private def generateSamples1D(count: Int): Array[Double] = {
		val array = new Array[Double](count)
		for (x <- 0 until count) array(x) = (x.toDouble + random.nextDouble) / count
		array
	}

	private def generateSamples2D(countX: Int, countY: Int): Array[(Double, Double)] = {
		val array = new Array[(Double, Double)](countX * countY)
		for (y <- 0 until countY; x <- 0 until countX)
			array(x + countX * y) = ((x.toDouble + random.nextDouble) / countX, (y.toDouble + random.nextDouble) / countY)
		array
	}
}
