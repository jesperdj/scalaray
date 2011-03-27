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
package org.jesperdj.scalaray

import common._
import lightsource._
import material._
import scene._
import shape._
import spectrum._
import texture._
import vecmath._

object Scene02 {
  def createScene(): Scene = {
    val mat1 = new MatteMaterial(new ConstantTexture(new Spectrum(0.5, 1.0, 1.0)))
    val mat2 = new MatteMaterial(new ConstantTexture(new Spectrum(1.0, 1.0, 1.0)))

//    val s1 = new Sphere(0.75, -1.0, 1.0, π * 4.0 / 3.0)
//    val p1 = new TransformedPrimitive(new GeometricPrimitive(s1, mat1), Transform.translate(0.0, 0.75, 4.0) * Transform.rotateY(π / 4.0) * Transform.rotateZ(π / 2.0))
    val p1 = new TransformedPrimitive(createCube(0.5, mat1), Transform.translate(0.0, 0.75, 4.0) * Transform.rotateY(-π / 6.0) * Transform.rotateX(-π / 6.0))

    val s2 = new Disk(3.0)
    val p2 = new TransformedPrimitive(new GeometricPrimitive(s2, mat2), Transform.translate(0.0, 0.0, 4.0) * Transform.rotateX(-π / 2.0))

    val l1 = new PointLightSource(new Point(0.5, 2.0, 0.0), new Spectrum(30.0, 30.0, 30.0))

    val s3 = new Disk(1.5)
    val t3 = Transform.translate(-0.3, 5.0, 3.5) * Transform.rotateX(π / 2.0)
    val l2 = new DiffuseAreaLightSource(s3, t3, new Spectrum(0.7, 0.7, 0.7), 4)
    val p3 = new TransformedPrimitive(new GeometricPrimitive(s3, mat1, l2), t3)

    new Scene(new CompositePrimitive(p1, p2, p3), l1 :: l2 :: Nil)
  }

  def createCube(size: Double, material: Material): Primitive = {
    new TransformedPrimitive(new GeometricPrimitive(Cube, material), Transform.scale(2.0 * size) * Transform.translate(-0.5, -0.5, -0.5))
  }
}
