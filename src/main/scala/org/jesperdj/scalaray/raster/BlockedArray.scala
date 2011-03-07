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

// Blocked array (pbrt A.2.5)
private final class BlockedArray[T : ClassManifest] (val sizeX: Int, val sizeY: Int, logBlockSize: Int = 4) {
  // Size of the side of a block
  private val blockSize = 1 << logBlockSize
  private val blockSizeSquared = blockSize * blockSize

  // Round up to a multiple of the block size
  private def roundUp(n: Int) = (n + blockSize - 1) & ~(blockSize - 1)

  // Number of blocks in horizontal direction
  private val blockCountX = roundUp(sizeX) >> logBlockSize

  // Array to contain the data
  private val data = new Array[T](roundUp(sizeX) * roundUp(sizeY))

  private def block(n: Int) = n >> logBlockSize
  private def offset(n: Int) = n & (blockSize - 1)

  // Compute the index of an (x, y) position into the data
  private def index(x: Int, y: Int) = blockSizeSquared * (blockCountX * block(y) + block(x)) + blockSize * offset(y) + offset(x)

  // Access an element by (x, y) position
  def apply(x: Int, y: Int) = data(index(x, y))

  // Update an element by (x, y) position
  def update(x: Int, y: Int, value: T): Unit = data(index(x, y)) = value

  override def toString = "BlockedArray(sizeX=%d, sizeY=%d, logBlockSize=%d)" format (sizeX, sizeY, logBlockSize)
}
