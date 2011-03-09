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
package org.jesperdj.scalaray.raster

// Rectangle in raster coordinates
final class Rectangle (val left: Int, val top: Int, val right: Int, val bottom: Int) {
  val width = right - left + 1
  val height = bottom - top + 1

  override def toString = "Rectangle(left=%d, top=%d, right=%d, bottom=%d)" format (left, top, right, bottom)
}

object Rectangle {
  // Create a rectangle
  def apply(left: Int, top: Int, right: Int, bottom: Int) = new Rectangle(left, top, right, bottom)

  // Create a rectangle with a width and height, with (left, top) = (0, 0)
  def apply(width: Int, height: Int) = new Rectangle(0, 0, width - 1, height - 1)

  // Extractor method
  def unapply(rectangle: Rectangle) = Some(rectangle.left, rectangle.top, rectangle.right, rectangle.bottom)
}
