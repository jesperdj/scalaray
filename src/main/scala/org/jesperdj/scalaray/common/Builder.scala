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

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

trait Accumulator[-T] {
  def +=(value: T): Unit
}

trait Builder[+T] {
  def build(): T
}

// TODO: What about co- and contravariance in ListBuilder? See Daniel Spiewak's answer on this StackOverflow question:
// http://stackoverflow.com/questions/663254/scala-covariance-contravariance-question

class ListBuilder[T] extends Accumulator[T] with Builder[List[T]] {
  private val buffer: ListBuffer[T] = ListBuffer.empty
  def +=(value: T): Unit = buffer += value
  def build: List[T] = buffer.toList
}
