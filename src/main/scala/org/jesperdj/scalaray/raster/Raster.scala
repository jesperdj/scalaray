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

import org.jesperdj.scalaray._

// Raster (mutable)
final class Raster (width: Int, height: Int) {
	// The pixels in the raster
	private val pixels = {
		val array = new BlockedArray[Pixel](width, height)
		for (y <- 0 until height; x <- 0 until width) array(x, y) = new Pixel
		array
	}

	// Access the pixel at position (x, y)
	def apply(x: Int, y: Int) = pixels(x, y)

	// Convert raster to an image (NOTE: for now a simplistic implementation without tone mapping)
	def toImage = {
		val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

		for (y <- 0 until height; x <- 0 until width) {
			val (red, green, blue) = this(x, y).spectrum.toRGB
			image.setRGB(x, y, (clamp(red, 0.0, 1.0) * 255.0).toInt << 16 | (clamp(green, 0.0, 1.0) * 255.0).toInt << 8 | (clamp(blue, 0.0, 1.0) * 255.0).toInt)
		}

		image
	}

	override def toString = "Raster(width=%d, height=%d, pixels=%s)" format (width, height, pixels)
}
