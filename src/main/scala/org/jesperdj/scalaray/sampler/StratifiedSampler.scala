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

import scala.collection.immutable.Traversable

import org.jesperdj.scalaray.raster.Rectangle

// Stratified sampler (pbrt 7.3)
final class StratifiedSampler (val rectangle: Rectangle, pixelsPerBatch: Int, samplesPerPixelX: Int, samplesPerPixelY: Int,
                               jitter: Boolean, samplePatternSpecs: Traversable[SamplePatternSpec]) extends Sampler {
  // Number of samples per pixel
  val samplesPerPixel = samplesPerPixelX * samplesPerPixelY

  // Number of batches
  val numberOfBatches = ((rectangle.width * rectangle.height) / pixelsPerBatch.toFloat).ceil.toInt

  // Index of the next batch to generate
  private var nextBatchIndex = 0

  // Check if there is a next batch
  def hasNext = nextBatchIndex < numberOfBatches

  // Generate the next batch
  def next(): SampleBatch = {
    if (!hasNext) throw new NoSuchElementException("next on empty iterator")

    val batch = new SampleBatchImpl(nextBatchIndex)
    nextBatchIndex += 1
    batch
  }

  // Implementation of SampleBatch
  private class SampleBatchImpl (val batchIndex: Int) extends SampleBatch {
    // Number of pixels left to generate samples for
    private var pixelsLeft = if (batchIndex < numberOfBatches - 1) pixelsPerBatch else {
      // The last batch contains the remaining pixels (can be less than pixelsPerBatch)
      rectangle.width * rectangle.height - (numberOfBatches - 1) * pixelsPerBatch
    }

    // Indices of the current pixel
    private var (pixelIndexX, pixelIndexY) = {
      val pixelIndex = batchIndex * pixelsPerBatch
      (rectangle.left + pixelIndex % rectangle.width, rectangle.top + pixelIndex / rectangle.width)
    }

    // Samples for the current pixel
    private var pixelSamples: Array[Sample] = generatePixelSamples(pixelIndexX, pixelIndexY)

    // Index of the next sample
    private var sampleIndex = 0

    private var moreSamples = true

    // Check if there is a next sample
    def hasNext = moreSamples

    // Generate the next sample
    def next() = {
      if (!hasNext) throw new NoSuchElementException("next on empty iterator")

      val sample = pixelSamples(sampleIndex)

      // Move indices to the next sample
      sampleIndex += 1
      if (sampleIndex >= samplesPerPixel) {
        sampleIndex = 0; pixelIndexX += 1
        if (pixelIndexX > rectangle.right) {
          pixelIndexX = rectangle.left; pixelIndexY += 1
        }

        if (pixelsLeft > 0) {
          // Generate samples for the next pixel
          pixelSamples = generatePixelSamples(pixelIndexX, pixelIndexY)
        }
        else {
          // No more samples in this batch
          moreSamples = false
        }
      }

      sample
    }

    // Generate samples for pixel (px, py)
    private def generatePixelSamples(px: Int, py: Int): Array[Sample] = {
      val samples = new Array[Sample](samplesPerPixel)

      // Generate image, lens and time samples for this pixel
      val imageSamples = StratifiedSampler.generateSamplePattern2D(samplesPerPixelX, samplesPerPixelY, jitter)
      val lensSamples = StratifiedSampler.generateSamplePattern2D(samplesPerPixelX, samplesPerPixelY, jitter)
      val timeSamples = StratifiedSampler.generateSamplePattern1D(samplesPerPixel, jitter)

      for (i <- 0 until samplesPerPixel) {
        import scala.collection.mutable.{ Map => MutableMap }

        // Generate 1D and 2D sample patterns for the current sample
        val samplePatterns1D = MutableMap[Int, SamplePattern1D]()
        val samplePatterns2D = MutableMap[Int, SamplePattern2D]()

        samplePatternSpecs foreach {
          _ match {
            case spec: SamplePatternSpec1D => samplePatterns1D += spec.id -> StratifiedSampler.generateSamplePattern1D(spec.count, jitter)
            case spec: SamplePatternSpec2D => samplePatterns2D += spec.id -> StratifiedSampler.generateSamplePattern2D(spec.count, jitter)
          }
        }

        val (ix, iy) = imageSamples(i)
        val (lu, lv) = lensSamples(i)
        val t = timeSamples(i)

        // Create Sample object, shift image samples to pixel position
        samples(i) = Sample(CameraSample(px + ix, py + iy, lu, lv, t), samplePatterns1D.toMap, samplePatterns2D.toMap)
      }

      pixelsLeft -= 1

      samples
    }
  }

  override def toString() = "StratifiedSampler"
}

object StratifiedSampler {
  import org.jesperdj.scalaray.common._

  // Random number generator
  private val rng: RandomNumberGenerator = ThreadSafeMersenneTwister

  // Generate a stratified 1D sample pattern
  private def generateSamplePattern1D(count: Int, jitter: Boolean): SamplePattern1D = {
    val array = new Array[Double](count)
    for (x <- 0 until count) array(x) = (x + (if (jitter) rng.nextDouble() else 0.5)) / count

    // Shuffle samples to decorrelate dimensions
    arrayToIndexedSeq(shuffle(array, rng))
  }

  // Generate a stratified 2D sample pattern
  private def generateSamplePattern2D(countX: Int, countY: Int, jitter: Boolean): SamplePattern2D = {
    val array = new Array[(Double, Double)](countX * countY)
    for (y <- 0 until countY; x <- 0 until countX)
      array(x + countX * y) = ((x + (if (jitter) rng.nextDouble() else 0.5)) / countX, (y + (if (jitter) rng.nextDouble() else 0.5)) / countY)

    // Shuffle samples to decorrelate dimensions
    arrayToIndexedSeq(shuffle(array, rng))
  }

  // Generate a stratified 2D sample pattern using Latin hypercube sampling
  private def generateSamplePattern2D(count: Int, jitter: Boolean): SamplePattern2D = {
    val array = new Array[(Double, Double)](count)
    for (i <- 0 until count) array(i) = ((i + (if (jitter) rng.nextDouble() else 0.5)) / count, (i + (if (jitter) rng.nextDouble() else 0.5)) / count)

    // Swap functions to swap the X or Y components of two samples
    def swapX(a: (Double, Double), b: (Double, Double)): ((Double, Double), (Double, Double)) = ((b._1, a._2), (a._1, b._2))
    def swapY(a: (Double, Double), b: (Double, Double)): ((Double, Double), (Double, Double)) = ((a._1, b._2), (b._1, a._2))

    // Shuffle along both dimensions independently
    arrayToIndexedSeq(shuffle(shuffle(array, rng, swapX), rng, swapY))
  }
}
