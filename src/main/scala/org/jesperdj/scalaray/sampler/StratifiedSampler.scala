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
final class StratifiedSampler (rectangle: Rectangle, pixelsPerBatch: Int, samplesPerPixelX: Int, samplesPerPixelY: Int,
                               jitter: Boolean, samplePatternSpecs: Traversable[SamplePatternSpec]) extends Sampler {
  // Number of samples per pixel
  private val samplesPerPixel = samplesPerPixelX * samplesPerPixelY

  // Number of samples per batch
  private val samplesPerBatch = samplesPerPixel * pixelsPerBatch

  // Batches produced by this sampler
  val batches = new Traversable[SampleBatch] {
    // Apply a function to each batch
    def foreach[U](f: SampleBatch => U): Unit = for (i <- 0 until size) f(new SampleBatchImpl(i))

    // Number of batches
    override val size = ((rectangle.width * rectangle.height) / pixelsPerBatch.toFloat).ceil.toInt
  }

  // Implementation of SampleBatch
  private class SampleBatchImpl (val batchIndex: Int) extends SampleBatch {
    // Samples in this batch
    val samples = new Traversable[Sample] {
      // Apply a function to each sample
      def foreach[U](f: Sample => U): Unit = {
        // Index of the first pixel in this batch
        val pixelIndex = batchIndex * pixelsPerBatch

        // Indices of the current pixel
        var py = rectangle.top + pixelIndex / rectangle.width
        var px = rectangle.left + pixelIndex % rectangle.width

        // Generate samples for each pixel
        0 until pixelsThisBatch foreach { _ =>
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

            val imageSample = imageSamples(i)
            val lensSample = lensSamples(i)
            val timeSample = timeSamples(i)

            // Create Sample object, shift image samples to pixel position, apply the function to the sample
            f(new Sample(px + imageSample._1, py + imageSample._2, lensSample._1, lensSample._2, timeSample, samplePatterns1D.toMap, samplePatterns2D.toMap))
          }

          // Move indices to the next pixel
          px += 1; if (px > rectangle.right) { px = rectangle.left; py += 1 }
        }
      }

      // Number of pixels in this batch
      private val pixelsThisBatch = if (batchIndex < batches.size - 1) pixelsPerBatch else {
        // The last batch contains the remaining pixels (can be less than pixelsPerBatch)
        rectangle.width * rectangle.height - (batches.size - 1) * pixelsPerBatch
      }

      // Number of samples in this batch
      override val size = samplesPerPixel * pixelsThisBatch
    }
  }

  override def toString = "StratifiedSampler"
}

object StratifiedSampler {
  import org.jesperdj.scalaray.common._

  private val random = new scala.util.Random

  // Generate a stratified 1D sample pattern
  private def generateSamplePattern1D(count: Int, jitter: Boolean): SamplePattern1D = {
    val array = new Array[Double](count)
    for (x <- 0 until count) array(x) = (x + (if (jitter) random.nextDouble else 0.5)) / count

    // Shuffle samples to decorrelate dimensions
    arrayToIndexedSeq(shuffle(array))
  }

  // Generate a stratified 2D sample pattern
  private def generateSamplePattern2D(countX: Int, countY: Int, jitter: Boolean): SamplePattern2D = {
    val array = new Array[(Double, Double)](countX * countY)
    for (y <- 0 until countY; x <- 0 until countX)
      array(x + countX * y) = ((x + (if (jitter) random.nextDouble else 0.5)) / countX, (y + (if (jitter) random.nextDouble else 0.5)) / countY)

    // Shuffle samples to decorrelate dimensions
    arrayToIndexedSeq(shuffle(array))
  }

  // Generate a stratified 2D sample pattern using Latin hypercube sampling
  private def generateSamplePattern2D(count: Int, jitter: Boolean): SamplePattern2D = {
    val array = new Array[(Double, Double)](count)
    for (i <- 0 until count) array(i) = ((i + (if (jitter) random.nextDouble else 0.5)) / count, (i + (if (jitter) random.nextDouble else 0.5)) / count)

    // Swap functions to swap the X or Y components of two samples
    def swapX(a: (Double, Double), b: (Double, Double)): ((Double, Double), (Double, Double)) = ((b._1, a._2), (a._1, b._2))
    def swapY(a: (Double, Double), b: (Double, Double)): ((Double, Double), (Double, Double)) = ((a._1, b._2), (b._1, a._2))

    // Shuffle along both dimensions independently
    arrayToIndexedSeq(shuffle(shuffle(array, swapX), swapY))
  }
}
