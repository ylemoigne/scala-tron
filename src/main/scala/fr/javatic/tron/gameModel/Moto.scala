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

package fr.javatic.tron.gameModel

import java.awt.Graphics2D
import scala.collection.mutable.ListBuffer
import fr.javatic.tron.{Direction, Drawable, mathModel}
import fr.javatic.tron.mathModel.Point

class Moto(var vector: mathModel.Vector) extends Drawable {
  private val paths: ListBuffer[mathModel.Vector] = ListBuffer(vector)

  def forward(): Unit = {
    val currentVector = paths.head
    val newVector = currentVector.copy(length = currentVector.length + 1)
    paths.update(0, newVector)
  }

  def turn(newDirection: Direction.DirectionValue): Unit = {
    mathModel.Vector(Point(paths.head.end.x, paths.head.end.y), 0, newDirection.angle) +=: paths
  }

  def currentPosition = {
    paths.head.end
  }

  def crossMyPaths(p: Point) = {
    paths.exists {
      _.intersect(p).isDefined
    }
  }

  def teleport(position: Point): Unit = {
    mathModel.Vector(position, 0, paths.head.angle) +=: paths
  }

  def draw(g: Graphics2D): Unit = {
    paths.foreach {
      _.draw(g)
    }
  }
}
