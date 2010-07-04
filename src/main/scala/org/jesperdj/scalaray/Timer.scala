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

class Timer (val name: String) {
	private var totalTime: Long = 0L
	private var startTime: Long = 0L

	def start() { startTime = System.nanoTime }
	def stop() { totalTime += System.nanoTime - startTime }

	def time[T](block: => T): T = { start; val result: T = block; stop; result }

	def total = totalTime

	override def toString = "%s: %g seconds" format (name, totalTime / 1e9)
}
