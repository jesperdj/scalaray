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

import java.io.File
import javax.imageio.ImageIO

import scala.actors.Futures._
import scala.collection.immutable.{ IndexedSeq, Traversable }

import org.jesperdj.scalaray.camera._
import org.jesperdj.scalaray.filter._
import org.jesperdj.scalaray.integrator._
import org.jesperdj.scalaray.lightsource._
import org.jesperdj.scalaray.material._
import org.jesperdj.scalaray.raster._
import org.jesperdj.scalaray.reflection._
import org.jesperdj.scalaray.renderer._
import org.jesperdj.scalaray.sampler._
import org.jesperdj.scalaray.scene._
import org.jesperdj.scalaray.shape._
import org.jesperdj.scalaray.spectrum._
import org.jesperdj.scalaray.util._
import org.jesperdj.scalaray.vecmath._

object Main {
	def main(args: Array[String]) {
		println("ScalaRay - Ray tracer based on pbrt (see http://pbrt.org) written in Scala 2.8")
		println("Copyright (C) 2009, 2010  Jesper de Jong")

		println()
		println("Setup...")
		val scene = createScene()

		val rect = new Rectangle(0, 0, 799, 599)

		val filter: Filter = new MitchellFilter
		val raster = new Raster(rect, filter)

		val surfaceIntegrator: SurfaceIntegrator = DirectLightingSurfaceIntegrator(scene)
		val volumeIntegrator: VolumeIntegrator = VacuumVolumeIntegrator
		val sampler: Sampler = new StratifiedSampler(rect, 2, 2, surfaceIntegrator.sampleSpecs ++ volumeIntegrator.sampleSpecs)
		val renderer: Renderer = new SamplerRenderer(scene, sampler, surfaceIntegrator, volumeIntegrator)

		val camera: Camera = new PerspectiveCamera(Transform.translate(0.0f, 0.75f, 0.0f), π / 4.0f, rect.width, rect.height)

		println("- Surface integrator: " + surfaceIntegrator)
		println("- Volume integrator: " + volumeIntegrator)
		println("- Sampler: " + sampler)
		println("- Filter: " + filter)
		println("- Renderer: " + renderer)
		println("- Camera: " + camera)

		println()
		println("Rendering...")
		val timer = new Timer("Total rendering time")
		timer.time { renderer.render(camera, raster) }
		println(timer.toString)

		ImageIO.write(raster.toImage, "png", new File("output.png"))

		println()
		println("Finished")
	}

	def createScene(): Scene = {
		val simpleBxDFs: IndexedSeq[BxDF] = IndexedSeq(new Lambertian(Spectrum.Unit))

		val simpleMaterial = new Material {
			def bsdf(dgGeom: DifferentialGeometry, dgShading: DifferentialGeometry): BSDF = new BSDF(simpleBxDFs, dgShading, dgGeom.normal)
		}

//		val s1 = new Sphere(0.75f, -1.0f, 1.0f, π * 4.0f / 3.0f)
//		val p1 = new TransformedPrimitive(new GeometricPrimitive(s1, simpleMaterial), Transform.translate(0.0f, 0.75, 4.0f) * Transform.rotateY(π / 4.0f) * Transform.rotateZ(π / 2.0f))
		val p1 = new TransformedPrimitive(createCube(0.5f, simpleMaterial), Transform.translate(0.0f, 0.75f, 4.0f) * Transform.rotateY(-π / 6.0f) * Transform.rotateX(-π / 6.0f))

		val s2 = new Disk(3.0f)
		val p2 = new TransformedPrimitive(new GeometricPrimitive(s2, simpleMaterial), Transform.translate(0.0f, 0.0f, 4.0f) * Transform.rotateX(-π / 2.0f))

//		val l1 = new DirectionalLightSource(new Vector(-0.5f, -1.25f, 4.0f), new Spectrum(0.4f, 0.4f, 0.4f))
		val l1 = new PointLightSource(new Point(0.5f, 2.0f, 0.0f), new Spectrum(30.0f, 30.0f, 30.0f))

		val s3 = new Disk(1.5f)
		val t3 = Transform.translate(-0.3f, 5.0f, 3.5f) * Transform.rotateX(π / 2.0f)
		val l2 = new AreaLightSource(s3, t3, new Spectrum(0.7f, 0.7f, 0.7f), 10)
		val p3 = new TransformedPrimitive(new GeometricPrimitive(l2, simpleMaterial), t3)

		new Scene(new CompositePrimitive(p1, p2, p3), Traversable(l1, l2))
	}

	def createCube(size: Float, material: Material): Primitive = {
		import scala.collection.immutable.IndexedSeq

		// TODO: Properly generate normals and u, v surface coordinates for vertices

		// Generate vertices
		val v: IndexedSeq[Vertex] = for (i <- 0 to 7) yield
			new Vertex(Point(if ((i & 1) != 0) size else -size, if ((i & 2) != 0) size else -size, if ((i & 4) != 0) size else -size), Normal.Zero, 0.0f, 0.0f)

		// Front
		val ft1 = new GeometricPrimitive(new Triangle(v(0), v(2), v(3)), material)
		val ft2 = new GeometricPrimitive(new Triangle(v(3), v(1), v(0)), material)
		val f = new CompositePrimitive(ft1, ft2)

		// Back
		val bkt1 = new GeometricPrimitive(new Triangle(v(5), v(7), v(6)), material)
		val bkt2 = new GeometricPrimitive(new Triangle(v(6), v(4), v(5)), material)
		val bk = new CompositePrimitive(bkt1, bkt2)

		// Left
		val lt1 = new GeometricPrimitive(new Triangle(v(4), v(6), v(2)), material)
		val lt2 = new GeometricPrimitive(new Triangle(v(2), v(0), v(4)), material)
		val l = new CompositePrimitive(lt1, lt2)

		// Right
		val rt1 = new GeometricPrimitive(new Triangle(v(1), v(3), v(7)), material)
		val rt2 = new GeometricPrimitive(new Triangle(v(7), v(5), v(1)), material)
		val r = new CompositePrimitive(rt1, rt2)

		// Bottom
		val btt1 = new GeometricPrimitive(new Triangle(v(4), v(0), v(1)), material)
		val btt2 = new GeometricPrimitive(new Triangle(v(1), v(5), v(4)), material)
		val bt = new CompositePrimitive(btt1, btt2)

		// Top
		val tt1 = new GeometricPrimitive(new Triangle(v(7), v(3), v(2)), material)
		val tt2 = new GeometricPrimitive(new Triangle(v(2), v(6), v(7)), material)
		val t = new CompositePrimitive(tt1, tt2)

		new CompositePrimitive(f, bk, l, r, bt, t)
	}
}
