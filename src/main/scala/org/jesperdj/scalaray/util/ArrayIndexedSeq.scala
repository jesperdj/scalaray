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
package org.jesperdj.scalaray.util

import scala.collection.immutable.IndexedSeq

// IndexedSeq which wraps an array and which is @specialized on Float. This is much more efficient than converting an array to a standard IndexedSeq,
// and standard Scala 2.8 collections are also not @specialized, which means you'd get a lot of unnecessary boxing and unboxing.
final class ArrayIndexedSeq[@specialized(Float) T] (array: Array[T]) extends IndexedSeq[T] {
	def apply(idx: Int): T = array(idx)
	def length: Int = array.length
}

object ArrayIndexedSeq {
	def apply[T](array: Array[T]): ArrayIndexedSeq[T] = new ArrayIndexedSeq(array)
}
