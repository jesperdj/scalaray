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
package org.jesperdj.scalaray

import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

package object scene {
	// Implicit conversion for transforming an intersection
	implicit def implicitTransformIntersection(t: Transform) = new MultipliableSame[Intersection] {
		def *(i: Intersection): Intersection = new Intersection(t * i.dg, i.distance, i.primitive)
	}
}
