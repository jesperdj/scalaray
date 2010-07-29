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
package org.jesperdj.scalaray.raster

import java.awt.image.BufferedImage

import org.jesperdj.scalaray.filter.Filter
import org.jesperdj.scalaray.sampler.CameraSample
import org.jesperdj.scalaray.spectrum._
import org.jesperdj.scalaray.util._

// Raster (mutable)
final class PixelRaster (rectangle: Rectangle, filter: Filter) extends Raster(rectangle) {
	import PixelRaster._

	// The pixels in the raster
	private val pixels = {
		val array = new BlockedArray[Pixel](rectangle.width, rectangle.height)
		for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) array(x - rectangle.left, y - rectangle.top) = new Pixel
		array
	}

	def addSample(sample: CameraSample, spectrum: Spectrum): Unit = {
		// Determine the raster extent of the sample
		val ix = sample.imageX - 0.5f
		val iy = sample.imageY - 0.5f

		val minX = math.max((ix - filter.extentX).ceil.toInt, rectangle.left)
		val maxX = math.min((ix + filter.extentX).floor.toInt, rectangle.right)
		val minY = math.max((iy - filter.extentY).ceil.toInt, rectangle.top)
		val maxY = math.min((iy + filter.extentY).floor.toInt, rectangle.bottom)

		// Add radiance to relevant pixels in the raster, weighted by reconstruction filter
		for (y <- minY to maxY; x <- minX to maxX) pixels(x, y).add(spectrum, filter(x - ix, y - iy))
	
	}

	// Convert raster to an image (NOTE: for now a simplistic implementation without tone mapping)
	def toImage = {
		val image = new BufferedImage(rectangle.width, rectangle.height, BufferedImage.TYPE_INT_RGB)

		for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) {
			val px = x - rectangle.left; val py = y - rectangle.top
			val (red, green, blue) = pixels(px, py).spectrum.toRGB
			image.setRGB(px, py, (clamp(red, 0.0f, 1.0f) * 255.0f).toInt << 16 | (clamp(green, 0.0f, 1.0f) * 255.0f).toInt << 8 | (clamp(blue, 0.0f, 1.0f) * 255.0f).toInt)
		}

		image
	}

	override def toString = "PixelRaster(rectangle=%s, filter=%s, pixels=%s)" format (rectangle, filter, pixels)
}

object PixelRaster {
	// Pixel (mutable)
	private final class Pixel {
		private var value = Spectrum.Black
		private var weight = 0.0f

		// Add a spectrum with the specified weight to this pixel
		def add(s: Spectrum, w: Float): Unit = synchronized { value +*= (s, w); weight += w }

		// Get the spectrum of this pixel
		def spectrum: Spectrum = if (weight == 0.0f) Spectrum.Black else value / weight

		override def toString = "Pixel(value=%s, weight=%g)" format (value, weight)
	}
}
