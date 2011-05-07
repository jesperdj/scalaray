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
package org.jesperdj.scalaray.common

// Interval
final class Interval (val min: Double, val max: Double) {
  // Center of the interval
  def center = (min + max) / 2.0

  // Length of the interval
  def length = max - min

  // Check if a value is inside the interval
  def inside(n: Double) = n >= min && n <= max

  // Check if another interval overlaps with this one
  def overlaps(i: Interval) = i.max >= min && i.min <= max
}

object Interval {
  def apply(min: Double, max: Double) = new Interval(min, max)
}
