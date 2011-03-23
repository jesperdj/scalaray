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
package org.jesperdj.scalaray.reflection

// TODO: This is a quick, ugly and inefficient translation of pbrt's C++ code for now. Improve this.

import org.jesperdj.scalaray.common._
import org.jesperdj.scalaray.spectrum._

trait Fresnel {
  def apply(cosThetaI: Double): Spectrum
}

final class FresnelConductor (eta: Spectrum, k: Spectrum) extends Fresnel {
  def apply(cosThetaI: Double): Spectrum = Fresnel.conductorReflectance(cosThetaI.abs, eta, k)
}

final class FresnelDielectric (etaI: Double, etaT: Double) extends Fresnel {
  def apply(cosThetaI: Double): Spectrum = {
    val cti = clamp(cosThetaI, -1.0, 1.0)
    val (ei, et) = if (cosThetaI > 0.0) (etaI, etaT) else (etaT, etaI)
    val sint = ei / et * math.sqrt(math.max(0.0, 1.0 - cti * cti))
    if (sint >= 1.0) Spectrum.Unit else {
      val cost = math.sqrt(math.max(0.0, 1.0 - sint * sint))
      Fresnel.dielectricReflectance(cti.abs, cost, new Spectrum(ei, ei, ei), new Spectrum(et, et, et))
    }

    Spectrum.Black
  }
}

object Fresnel {
  // Fresnel reflectance for a dielectric (pbrt 8.2.1)
  def dielectricReflectance(cosThetaI: Double, cosThetaT: Double, etaI: Spectrum, etaT: Spectrum): Spectrum = {
    val etci = etaT * cosThetaI
    val eict = etaI * cosThetaT
    val parl = (etci - eict) / (etci + eict)

    val eici = etaI * cosThetaI
    val etct = etaT * cosThetaT
    val perp = (eici - etct) / (eici + etct)

    (parl * parl + perp * perp) / 2.0
  }

  // Fresnel reflectance for a conductor (pbrt 8.2.1)
  def conductorReflectance(cosThetaI: Double, eta: Spectrum, k: Spectrum): Spectrum = {
    val e2k2 = eta * eta + k * k
    val ci2 = cosThetaI * cosThetaI

    val tmp = e2k2 * ci2
    val parl2 = (tmp - (2.0 * eta * cosThetaI) + Spectrum.Unit) / (tmp + (2.0 * eta * cosThetaI) + Spectrum.Unit)

    val cis = new Spectrum(ci2, ci2, ci2)
    val perp2 = (e2k2 - (2.0 * eta * cosThetaI) + cis) / (e2k2 + (2.0 * eta * cosThetaI) + cis)

    (parl2 + perp2) / 2.0
  }
}
