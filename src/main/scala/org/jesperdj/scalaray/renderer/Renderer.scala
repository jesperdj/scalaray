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
package org.jesperdj.scalaray.renderer

// NOTE: In pbrt, the design of Renderer is messy because it doesn't only do rendering, it also traces rays.
// In ScalaRay, the tracing of rays is implemented in Integrator, which leads to a cleaner design in which
// there is no dependency from implementations of SurfaceIntegrator and VolumeIntegrator to Renderer.

// Renderer (pbrt 1.3.3)
trait Renderer {
  def render(): PixelBuffer
}
