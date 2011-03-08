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
import org.jesperdj.scalaray.vecmath.{ Ray, RayDifferential }

// NOTE: In pbrt, the camera contains the Film. This is a nice analogy with how old photo cameras work, but the camera itself doesn't do anything
// with the film, so from an object oriented design point of view there's no good reason why the camera should contain it.
// The only responsibility of Camera is to generate camera rays.

// Camera (pbrt 6.1)
abstract class Camera {
  // Generate a camera ray for a sample (pbrt 6.1)
  def generateRay(sample: CameraSample): Ray

  // Generate a camera ray with differentials
  def generateRayDifferential(sample: CameraSample, scale: Double = 1.0): RayDifferential = {
    val ray = generateRay(sample)

    val rx = generateRay(new CameraSample(sample.imageX + 1, sample.imageY, sample.lensU, sample.lensV, sample.time))
    val ry = generateRay(new CameraSample(sample.imageX, sample.imageY + 1, sample.lensU, sample.lensV, sample.time))

    val rxOrigin = ray.origin + (rx.origin - ray.origin) * scale
    val rxDirection = ray.direction + (rx.direction - ray.direction) * scale

    val ryOrigin = ray.origin + (ry.origin - ray.origin) * scale
    val ryDirection = ray.direction + (ry.direction - ray.direction) * scale

    RayDifferential(ray, rxOrigin, rxDirection, ryOrigin, ryDirection)
  }
}
