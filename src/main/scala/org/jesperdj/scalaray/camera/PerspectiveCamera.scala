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
package org.jesperdj.scalaray.camera

import org.jesperdj.scalaray.sampler.CameraSample
import org.jesperdj.scalaray.vecmath._

// Perspective camera (pbrt 6.2.2) - This is less complicated than the pbrt version
final class PerspectiveCamera (cameraToWorld: Transform, angleOfView: Float, rasterWidth: Int, rasterHeight: Int) extends Camera {
  require(!cameraToWorld.hasScale, "PerspectiveCamera only works correctly when cameraToWorld transform has no scale factor")

  private val tx = (rasterWidth - 1) / 2.0f
  private val ty = (rasterHeight - 1) / 2.0f

  private val s = 2.0f * math.tan(angleOfView / 2.0f).toFloat / rasterWidth

  private val cameraOrigin = cameraToWorld * Point.Origin

  // Generate a camera ray for a sample (pbrt 6.2.2)
  def generateRay(sample: CameraSample): Ray =
    new Ray(cameraOrigin, (cameraToWorld * new Vector((sample.imageX - tx) * s, (ty - sample.imageY) * s, 1.0f)).normalize)

  override def toString =
    "PerspectiveCamera(cameraToWorld=%s, angleOfView=%g, rasterWidth=%d, rasterHeight=%d)" format (cameraToWorld, angleOfView, rasterWidth, rasterHeight)
}
