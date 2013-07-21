/*
 * Copyright (c) 2013, Yann Le Moigne
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package fr.javatic.tron.mathModel

import java.awt.Graphics2D
import fr.javatic.tron.Drawable
import scala.util.Try

case class Vector(origin: Point, length: Int, angle: Double) extends Drawable {
  val end: Point = Point(origin.x + projectionLengthOnX.toInt, origin.y + projectionLengthOnY.toInt)

  def projectionLengthOnX = math.cos(angle) * length

  def projectionLengthOnY = math.sin(angle) * length

  def projectionOnX = Vector(Point(origin.x, 0), projectionLengthOnX.toInt, 0)

  def projectionOnY = Vector(Point(0, origin.y), projectionLengthOnY.toInt, math.Pi / 2)

  def draw(g: Graphics2D): Unit = {
    g.drawLine(origin.x, origin.y, end.x, end.y)
  }

  def dotProduct(p: Point) = (p.x - origin.x) * (end.x - origin.x) + (p.y - origin.y) * (end.y - origin.y)

  // Paul Bourke algorithm : http://paulbourke.net/geometry/pointlineplane/
  // Plus fix from http://stackoverflow.com/a/2255848/1016547 for coincident line and 1D intersection
  // Plus my own modification, because if it's a 1D overlap, i want the overlap point from v1.end to v2.origin
  def intersect(that: Vector): Option[List[Point]] = {
    if (this.isPoint) {
      toPoint.get.intersect(that)
    } else if (that.isPoint) {
      intersect(that.toPoint.get)
    } else {
      intersectVector(that)
    }
  }

  def intersectVector(that: Vector): Option[List[Point]] = {
    val lineIntersection = new PaulBourkeLineIntersectionEquationSystem(this, that)
    lineIntersection.result match {
      case Result.Coincident => oneDimensionIntersect(that)
      case Result.Parallel => None
      case Result.Other => lineIntersection.segmentIntersectionPoint match {
        case None => None
        case Some(s: Point) => Option(List(s))
      }
    }
  }

  private def oneDimensionIntersect(b: Vector): Option[List[Point]] = {
    val a1 = end
    val a2 = origin
    val b1 = b.origin
    val b2 = b.end

    val vA2A1 = Vector(a2, a1)
    val denomx = vA2A1.projectionLengthOnX
    val denomy = vA2A1.projectionLengthOnY

    val ub = if (denomx.abs > denomy.abs)
               ((b1.x - a1.x) / denomx, (b2.x - a1.x) / denomx)
             else
               ((b1.y - a1.y) / denomy, (b2.y - a1.y) / denomy)

    val interval = overlapIntervals(ub._1.toInt, ub._2.toInt)

    if (interval.isEmpty) {
      None
    } else {
      Some(interval map {
        (f) =>
          val x = a2.x * f + a1.x * (1.0f - f)
          val y = a2.y * f + a1.y * (1.0f - f)
          Point(x.toInt, y.toInt)
      })
    }
  }

  private def overlapIntervals(ub1: Int, ub2: Int): List[Int] = {
    val a = 0 max (ub1 min ub2)
    val b = 1 min (ub1 max ub2)

    if (a > b) // no intersection
      Nil
    else if (a == b)
           List(a)
    else // if (A < B)
      List(a, b)
  }

  def intersect(p: Point): Option[List[Point]] = {
    if (p.isOnSegment(this)) {
      Some(List(p))
    } else {
      None
    }
  }

  def isPoint = {
    length == 0
  }

  def toPoint: Try[Point] = {
    Try {
      if (isPoint) {
        origin
      } else {
        throw new VectorIsNotAPointException(this)
      }
    }
  }

  // @see http://paulbourke.net/geometry/pointlineplane/
  // val name conformly to the bourke article for easier comparison 
  private class PaulBourkeLineIntersectionEquationSystem(val a: Vector, val b: Vector) {
    implicit def doubleToRangeCheck(v: Double) = new RangeCheck(v)

    private val p1 = a.origin
    private val p2 = a.end
    private val p3 = b.origin
    private val p4 = b.end

    private val denominator = (p4.y - p3.y) * (p2.x - p1.x) - (p4.x - p3.x) * (p2.y - p1.y)
    private val uaNumerator = (p4.x - p3.x) * (p1.y - p3.y) - (p4.y - p3.y) * (p1.x - p3.x)
    private val ubNumerator = (p2.x - p1.x) * (p1.x - p3.y) - (p2.y - p1.y) * (p1.x - p3.x)

    val areParallel = denominator == 0
    val areCoincident = (denominator == 0) && (uaNumerator == 0) && (ubNumerator == 0)

    private val uab = if (areParallel) None else Some((uaNumerator.toDouble / denominator, ubNumerator.toDouble / denominator))

    val lineIntersectionPoint = uab map {
      case (ua, ub) =>
        val intersectionX = origin.x + ua * (end.x - origin.x)
        val intersectionY = origin.y + ua * (end.y - origin.y)

        Point(intersectionX.toInt, intersectionY.toInt)
    }

    val aSegmentIntersectBLine = uab match {
      case Some(s) => s match {
        case (ua: Double, _) => ua in(0, 1, RangeCheckMode.Inclusive)
      }
      case None => false
    }

    val bSegmentIntersectALine = uab match {
      case Some(s) => s match {
        case (_, ub: Double) => ub in(0, 1, RangeCheckMode.Inclusive)
      }
      case None => false
    }

    val segmentIntersectionPoint = {
      if (aSegmentIntersectBLine && bSegmentIntersectALine) {
        lineIntersectionPoint
      } else {
        None
      }
    }

    val result = {
      if (areCoincident) {
        Result.Coincident
      } else if (areParallel) {
        Result.Parallel
      } else {
        Result.Other
      }
    }
  }

  private object Result extends Enumeration {
    val Coincident = Value
    val Parallel = Value
    val Other = Value
  }

}

object Vector {
  def apply(origin: Point, end: Point) = new Vector(origin, origin.distanceTo(end).toInt, end.angle)
}
