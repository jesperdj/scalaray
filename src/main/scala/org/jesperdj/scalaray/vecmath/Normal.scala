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

// NOTE: The main reason to have a separate class Normal (instead of using Vector for normals) is because normals
// have to be transformed differently: by multiplying them with the transpose of the inverse (see pbrt 2.8.3).

// Normal (pbrt 2.4)
final class Normal (val x: Double, val y: Double, val z: Double) {
  // Create a normal from a vector
  def this(v: Vector) = this(v.x, v.y, v.z)

  // Create a normal from a point
  def this(p: Point) = this(p.x, p.y, p.z)

  // Add two normals
  def +(n: Normal) = new Normal(x + n.x, y + n.y, z + n.z)

  // Subtract two normals
  def -(n: Normal) = new Normal(x - n.x, y - n.y, z - n.z)

  // Scale a normal
  def *(f: Double) = new Normal(x * f, y * f, z * f)
  def /(f: Double) = new Normal(x / f, y / f, z / f)

  // Unary minus
  def unary_- = new Normal(-x, -y, -z)

  // Dot product
  def *(n: Normal) = x * n.x + y * n.y + z * n.z

  // Dot product with a vector
  def *(v: Vector) = x * v.x + y * v.y + z * v.z

  // Cross product with a vector
  def **(v: Vector) = new Vector(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)

  // Length
  def length = math.sqrt(lengthSquared)
  def lengthSquared = this * this

  // Normalize
  def normalize = this / length

  // Get an element by index
  def apply(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def toString = "Normal(%g, %g, %g)" format (x, y, z)
}

object Normal {
  // Normal constants
  val XAxis = new Normal(1.0, 0.0, 0.0)
  val YAxis = new Normal(0.0, 1.0, 0.0)
  val ZAxis = new Normal(0.0, 0.0, 1.0)
  val Zero = new Normal(0.0, 0.0, 0.0)

  // Create a normal
  def apply(x: Double, y: Double, z: Double) = new Normal(x, y, z)

  // Create a normal from a vector
  def apply(v: Vector) = new Normal(v)

  // Create a normal from a point
  def apply(p: Point) = new Normal(p)

  // Extractor method
  def unapply(n: Normal) = Some(n.x, n.y, n.z)
}
