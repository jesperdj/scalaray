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

import org.jesperdj.scalaray.common._
import org.jesperdj.scalaray.filter.Filter
import org.jesperdj.scalaray.raster.{ Raster, Rectangle }
import org.jesperdj.scalaray.sampler.Sample
import org.jesperdj.scalaray.spectrum.Spectrum

// Pixel buffer
final class PixelBuffer (rectangle: Rectangle, filter: Filter) {
  private final class Pixel {
    private var totalSpectrum: Spectrum = Spectrum.Black
    private var totalWeight: Double = 0.0

    def +*=(spectrum: Spectrum, weight: Double): Unit = {
      totalSpectrum +*= (spectrum, weight)
      totalWeight += weight
    }

    def spectrum: Spectrum = if (totalWeight > 0.0) totalSpectrum / totalWeight else Spectrum.Black
  }

  private val pixels = {
    val raster = new Raster[Pixel](rectangle)
    for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) raster(x, y) = new Pixel
    raster
  }

  // Add the spectrum corresponding to a sample to the buffer
  def +=(sample: Sample, spectrum: Spectrum): Unit = {
    // Convert sample to image coordinates
    val ix = sample.imageX - 0.5
    val iy = sample.imageY - 0.5

    // Determine the pixels that are to be updated according to the extent of the filter
    val minX = math.max((ix - filter.extentX).ceil.toInt, rectangle.left)
    val maxX = math.min((ix + filter.extentX).floor.toInt, rectangle.right)
    val minY = math.max((iy - filter.extentY).ceil.toInt, rectangle.top)
    val maxY = math.min((iy + filter.extentY).floor.toInt, rectangle.bottom)

    // Update the relevant pixels
    for (y <- minY to maxY; x <- minX to maxX) pixels(x, y) +*= (spectrum, filter(x - ix, y - iy))
  }

  // Convert the pixels to an RGB image
  def toImage(): java.awt.image.BufferedImage = {
    def toByte(value: Double): Int = clamp(value * 255.0, 0.0, 255.0).toInt

    val image = new java.awt.image.BufferedImage(rectangle.width, rectangle.height, java.awt.image.BufferedImage.TYPE_INT_RGB)

    for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) {
      val (red, green, blue) = pixels(x, y).spectrum.toRGB
      image.setRGB(x - rectangle.left, y - rectangle.top, toByte(red) << 16 | toByte(green) << 8 | toByte(blue))
    }

    image
  }
}
