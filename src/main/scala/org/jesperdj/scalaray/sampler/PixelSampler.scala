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

import scala.collection.immutable.{ IndexedSeq, Stream, Traversable }

import org.jesperdj.scalaray.raster.Rectangle

// Sampler that generates samples for one pixel at a time
abstract class PixelSampler (rectangle: Rectangle, samplesPerPixelX: Int, samplesPerPixelY: Int) extends Sampler {
	protected val samplesPerPixel = samplesPerPixelX * samplesPerPixelY

	val samples: Traversable[Sample] = new SampleStream(rectangle.left, rectangle.top, 0, generateSamples(rectangle.left, rectangle.top))

	// Stream to generate samples
	private class SampleStream (x: Int, y: Int, index: Int, pixelSamples: IndexedSeq[Sample]) extends Stream[Sample] {
		def tailDefined = false

		override def isEmpty = y > rectangle.bottom

		override def head = pixelSamples(index)

		override def tail = {
			var xx = x; var yy = y; var ii = index + 1
			var ps = pixelSamples

			if (ii >= samplesPerPixel) {
				ii = 0; xx += 1
				if (xx > rectangle.right) { xx = rectangle.left; yy += 1 }
				if (yy <= rectangle.bottom) { ps = generateSamples(xx, yy) }
			}

			if (yy <= rectangle.bottom) new SampleStream(xx, yy, ii, ps) else Stream.Empty
		}
	}

	// Generate samples for one pixel
	protected def generateSamples(x: Int, y: Int): IndexedSeq[Sample]
}
