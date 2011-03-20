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
package org.jesperdj.scalaray.reflection

// BxDF type (pbrt 8.1)
final class BxDFType private (val flags: Int) {
  def &(other: BxDFType): BxDFType = new BxDFType(flags & other.flags)
  def |(other: BxDFType): BxDFType = new BxDFType(flags | other.flags)

  def unary_~ = new BxDFType(~flags)

  // Check if this BxDF type matches the given type
  def matches(other: BxDFType) = (flags & other.flags) == flags   // TODO: dubieus

  override def equals(other: Any): Boolean = other match {
    case that: BxDFType => (that canEqual this) && (flags == that.flags)
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[BxDFType]

  override def hashCode: Int = flags

  override def toString = "BxDFType(0x%02X)" format (flags)
}

object BxDFType {
  val None = new BxDFType(0)

  val Reflection = new BxDFType(1 << 0)
  val Transmission = new BxDFType(1 << 1)

  val Diffuse = new BxDFType(1 << 2)
  val Glossy = new BxDFType(1 << 3)
  val Specular = new BxDFType(1 << 4)

  val AllTypes = Diffuse | Glossy | Specular
  val AllReflection = Reflection | AllTypes
  val AllTransmission = Transmission | AllTypes
  val All = AllReflection | AllTransmission
}
