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
package org.jesperdj.scalaray.sampler

import org.jesperdj.scalaray.raster.Rectangle

// Low-discrepancy sampler (pbrt 7.4)
final class LowDiscrepancySampler (val rectangle: Rectangle, val samplesPerPixel: Int) extends Sampler {
  // TODO: Implement low-discrepancy sampler
  val numberOfBatches = 1
  def hasNext: Boolean = throw new UnsupportedOperationException("Not yet implemented")
  def next: SampleBatch = throw new UnsupportedOperationException("Not yet implemented")

  override def toString = "LowDiscrepancySampler"
}
