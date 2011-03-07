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
package org.jesperdj.scalaray.vecmath

// Point (pbrt 2.3)
final class Point (val x: Float, val y: Float, val z: Float) {
  // Create a point from a vector
  def this(v: Vector) = this(v.x, v.y, v.z)

  // Create a point from a normal
  def this(n: Normal) = this(n.x, n.y, n.z)

  // Add a vector to a point
  def +(v: Vector) = new Point(x + v.x, y + v.y, z + v.z)

  // Add a normal to a point
  def +(n: Normal) = new Point(x + n.x, y + n.y, z + n.z)

  // Subtract a vector from a point
  def -(v: Vector) = new Point(x - v.x, y - v.y, z - v.z)

  // Subtract a normal from a point
  def -(n: Normal) = new Point(x - n.x, y - n.y, z - n.z)

  // Subtract two points
  def -(p: Point) = new Vector(x - p.x, y - p.y, z - p.z)

  // Multiply or divide a point by a weight
  def *(f: Float) = new Point(x * f, y * f, z * f)
  def /(f: Float) = new Point(x / f, y / f, z / f)

  // Distance between two points
  def distance(p: Point) = (this - p).length
  def distanceSquared(p: Point) = (this - p).lengthSquared

  override def toString = "Point(%g, %g, %g)" format (x, y, z)
}

object Point {
  // Point constants
  val Origin = new Point(0.0f, 0.0f, 0.0f)
  val PositiveInfinity = new Point(Float.PositiveInfinity, Float.PositiveInfinity, Float.PositiveInfinity)
  val NegativeInfinity = new Point(Float.NegativeInfinity, Float.NegativeInfinity, Float.NegativeInfinity)

  // Create a point
  def apply(x: Float, y: Float, z: Float) = new Point(x, y, z)

  // Create a point from a vector
  def apply(v: Vector) = new Point(v)

  // Create a point from a normal
  def apply(n: Normal) = new Point(n)
}
