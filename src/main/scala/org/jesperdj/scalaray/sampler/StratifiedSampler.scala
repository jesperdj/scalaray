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

import org.jesperdj.scalaray.raster.Rectangle
import org.jesperdj.scalaray.util._

// Stratified sampler (pbrt 7.3)
final class StratifiedSampler (rectangle: Rectangle, samplesPerPixelX: Int, samplesPerPixelY: Int, sampleSpecs: Traversable[SampleSpec])
	extends PixelSampler(rectangle, samplesPerPixelX * samplesPerPixelY) {

	// Generate samples for one pixel
	protected def generateSamples(x: Int, y: Int): IndexedSeq[Sample] = {
		// Generate image, lens and time samples
		val imageSamples = StratifiedSampler.generateSamples2D(samplesPerPixelX, samplesPerPixelY)
		val lensSamples = StratifiedSampler.generateSamples2D(samplesPerPixelX, samplesPerPixelY)
		val timeSamples = StratifiedSampler.generateSamples1D(samplesPerPixel)

		for (i <- 0 until samplesPerPixel) yield {
			// Generate 1D and 2D sample patterns for the current sample
			val samples1D = MutableMap[Int, IndexedSeq[Float]]()
			val samples2D = MutableMap[Int, IndexedSeq[(Float, Float)]]()

			sampleSpecs foreach {
				_ match {
					case spec: SampleSpec1D => samples1D += spec.id -> (IndexedSeq() ++ StratifiedSampler.generateSamples1D(spec.count))
					case spec: SampleSpec2D => samples2D += spec.id -> (IndexedSeq() ++ StratifiedSampler.generateSamples2D(spec.count))
				}
			}

			val (ix, iy) = imageSamples(i)
			val (lu, lv) = lensSamples(i)

			// Create Sample object, shift image samples to pixel position
			new Sample(ix + x, iy + y, lu, lv, timeSamples(i), samples1D.toMap, samples2D.toMap)
		}
	}

	// Split the sampler into a number of sub-samplers (for multiprocessing)
	def split(count: Int): Traversable[Sampler] = {
		import scala.collection.mutable.ListBuffer

		val ratio: Float = rectangle.width.toFloat / rectangle.height.toFloat
		val pixels: Int = (rectangle.height.toFloat / math.sqrt(count / ratio).toFloat).ceil.toInt

		val samplers: ListBuffer[Sampler] = ListBuffer()

		for (py <- 0 until rectangle.height by pixels; px <- 0 until rectangle.width by pixels) {
			val rect = Rectangle(px, py, math.min(px + pixels - 1, rectangle.right), math.min(py + pixels - 1, rectangle.bottom))
			if (rect.width > 0 && rect.height > 0) samplers += new StratifiedSampler(rect, samplesPerPixelX, samplesPerPixelY, sampleSpecs)
		}

		require(samplers.size > 0, "Could not split sampler")

		samplers.toList
	}

	override def toString = "StratifiedSampler(rectangle=%s, samplesPerPixelX=%d, samplesPerPixelY=%d, sampleSpecs=%s)" format
		(rectangle, samplesPerPixelX, samplesPerPixelY, sampleSpecs)
}

object StratifiedSampler {
	// NOTE: scala.util.Random is thread-safe because it uses java.util.Random which is thread-safe
	private val random = new scala.util.Random

	// Generate a set of stratified 1D samples
	private def generateSamples1D(count: Int): Array[Float] = {
		val array = new Array[Float](count)

		// Generate stratified 1D samples
		for (x <- 0 until count) array(x) = (x.toFloat + random.nextFloat) / count

		// Shuffle samples to decorrelate dimensions
		shuffle(array)
	}

	// Generate a set of stratified 2D samples
	private def generateSamples2D(countX: Int, countY: Int): Array[(Float, Float)] = {
		val array = new Array[(Float, Float)](countX * countY)

		// Generate stratified 2D samples
		for (y <- 0 until countY; x <- 0 until countX)
			array(x + countX * y) = ((x.toFloat + random.nextFloat) / countX, (y.toFloat + random.nextFloat) / countY)

		// Shuffle samples to decorrelate dimensions
		shuffle(array)
	}

	// Generate a set of 2D samples using Latin hypercube sampling
	private def generateSamples2D(count: Int): Array[(Float, Float)] = {
		val array = new Array[(Float, Float)](count)

		// Generate Latin hypercube samples along diagonal
		for (i <- 0 until count) array(i) = ((i.toFloat + random.nextFloat) / count, (i.toFloat + random.nextFloat) / count)

		// Swap functions to swap the X or Y components of two samples
		def swapX(a: (Float, Float), b: (Float, Float)): ((Float, Float), (Float, Float)) = ((b._1, a._2), (a._1, b._2))
		def swapY(a: (Float, Float), b: (Float, Float)): ((Float, Float), (Float, Float)) = ((a._1, b._2), (b._1, a._2))

		// Shuffle along both dimensions independently
		shuffle(shuffle(array, swapX), swapY)
	}
}
