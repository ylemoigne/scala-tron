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


case class Point(x: Int, y: Int) {
  def translate(angle: Double, amount: Int) = Vector(this, amount, angle).end

  def projectionOnX = Point(x, 0)

  def projectionOnY = Point(0, y)

  val length = distanceTo(Point.Origin)

  def angle = {
    val angle = math.atan(y.toDouble / x)
    quadrant match {
      case Quadrant.UpRight => angle
      case Quadrant.UpLeft => angle + math.Pi
      case Quadrant.DownLeft => angle - math.Pi
      case Quadrant.DownRight => angle
    }
  }

  private def quadrant = {
    if (x >= 0 && y >= 0) {
      Quadrant.UpRight
    } else if (x < 0 && y >= 0) {
      Quadrant.UpLeft
    } else if (x < 0 && y < 0) {
      Quadrant.DownLeft
    } else {
      // x>0 && y<0
      Quadrant.DownRight
    }
  }

  def dotProduct(v: Vector) = {
    v.dotProduct(this)
  }

  def intersect(v: Vector) = {
    v.intersect(this)
  }

  def distanceTo(p: Point): Double = {
    val dx = p.x - x
    val dy = p.y - y
    math.sqrt(dx * dx + dy * dy)
  }

  def distanceTo(v: Vector): Double = {
    if (v.isPoint) {
      distanceTo(v.toPoint.get)
    } else {
      val distanceXToOriginX = v.origin.x - x
      val distanceYToOriginY = v.origin.y - y
      (v.projectionOnX.length * distanceYToOriginY - distanceXToOriginX * v.projectionOnY.length).abs / v.length.toDouble
    }
  }

  def isOnLine(v: Vector) = {
    distanceTo(v) == 0
  }

  def isOnSegment(v: Vector) = {
    if (isOnLine(v)) {
      val dotproduct = dotProduct(v)
      val squaredlength = v.length * v.length
      (dotproduct > 0) && (dotproduct < squaredlength)
    } else {
      false
    }
  }

  private object Quadrant extends Enumeration {
    val UpRight = Value
    val UpLeft = Value
    val DownLeft = Value
    val DownRight = Value
  }

}

object Point {
  val Origin = Point(0, 0)

  private def randomInt(max: Int): Int = (Math.random() * max).asInstanceOf[Int]

  def boundedRandom(maxWidth: Int, maxHeight: Int) = new Point(randomInt(maxWidth), randomInt(maxHeight))
}
