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
package org.jesperdj.scalaray.sampler

import scala.collection.immutable.{ IndexedSeq, Map }

// Camera sample
sealed class CameraSample (val imageX: Float, val imageY: Float, val lensU: Float, val lensV: Float, val time: Float) {
	override def toString = "CameraSample(imageX=%g, imageY=%g, lensU=%g, lensV=%g, time=%g)" format (imageX, imageY, lensU, lensV, time)
}

// Sample, a camera sample with 1D and 2D sample patterns
final class Sample (imageX: Float, imageY: Float, lensU: Float, lensV: Float, time: Float, val samples1D: Map[Int, IndexedSeq[Float]],
					val samples2D: Map[Int, IndexedSeq[(Float, Float)]]) extends CameraSample (imageX, imageY, lensU, lensV, time) {

	override def toString = "Sample(imageX=%g, imageY=%g, lensU=%g, lensV=%g, time=%g, samples1D=%s, samples2D=%s)" format
		(imageX, imageY, lensU, lensV, time, samples1D, samples2D)
}
