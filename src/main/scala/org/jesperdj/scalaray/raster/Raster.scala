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
import org.jesperdj.scalaray.spectrum.Spectrum
import org.jesperdj.scalaray.util._

// Raster (mutable)
final class Raster (val rectangle: Rectangle, filter: Filter) {
	// The pixels in the raster
	private val pixels = {
		val array = new BlockedArray[Pixel](rectangle.width, rectangle.height)
		for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) array(x - rectangle.left, y - rectangle.top) = new Pixel
		array
	}

	def addSample(sample: CameraSample, spectrum: Spectrum): Unit = {
		// Determine the raster extent of the sample
		val ix = sample.imageX - 0.5
		val iy = sample.imageY - 0.5

		val minX = math.max(math.ceil(ix - filter.extentX).toInt, rectangle.left)
		val maxX = math.min(math.floor(ix + filter.extentX).toInt, rectangle.right)
		val minY = math.max(math.ceil(iy - filter.extentY).toInt, rectangle.top)
		val maxY = math.min(math.floor(iy + filter.extentY).toInt, rectangle.bottom)

		// Add radiance to relevant pixels in the raster, weighted by reconstruction filter
		for (y <- minY to maxY; x <- minX to maxX) pixels(x, y).add(spectrum, filter(x - ix, y - iy))
	
	}

	// Convert raster to an image (NOTE: for now a simplistic implementation without tone mapping)
	def toImage = {
		val image = new BufferedImage(rectangle.width, rectangle.height, BufferedImage.TYPE_INT_RGB)

		for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) {
			val px = x - rectangle.left; val py = y - rectangle.top
			val (red, green, blue) = pixels(px, py).spectrum.toRGB
			image.setRGB(px, py, (clamp(red, 0.0, 1.0) * 255.0).toInt << 16 | (clamp(green, 0.0, 1.0) * 255.0).toInt << 8 | (clamp(blue, 0.0, 1.0) * 255.0).toInt)
		}

		image
	}

	override def toString = "Raster(rectangle=%s, filter=%s, pixels=%s)" format (rectangle, filter, pixels)
}
