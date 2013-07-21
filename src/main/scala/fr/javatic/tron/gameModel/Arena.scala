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

import fr.javatic.tron.{Colored, TimerActor, Direction}
import fr.javatic.tron.mathModel.{Point, Vector}
import scala.actors.Actor
import fr.javatic.tron.displayEngine.Renderer
import java.awt.Color

class Arena(val width: Int, val height: Int, val players: List[Player], val renderer: Renderer) extends Actor {

  case object ClockTick

  case object Stop

  private val timer = new TimerActor(20, this, ClockTick)
  private val playersMoto: Map[Player, Moto] = players.map(p => (p, createMoto(p.color))).toMap

  private def createMoto(c: Color) = {
    new Moto(new Vector(Point.boundedRandom(width, height), 0, Direction.random.angle)) with Colored {
      color = c
    }
  }

  private def pointIsOnBoard(p: Point) = {
    p.x >= 0 && p.x <= width && p.y >= 0 && p.y <= height
  }

  override def start() = {
    timer.start()
    if (getState == Actor.State.Terminated)
      restart()
    else
      super.start()

    this
  }

  def act(): Unit = {
    loop {
      react {
        case m: ChangeDirection =>
          playersMoto(m.player).turn(m.direction)
        case ClockTick =>
          playersMoto.foreach {
            case (player, moto) =>
              val currentPosition = moto.currentPosition
              moto.forward()

              val newPosition = moto.currentPosition
              if (!pointIsOnBoard(newPosition)) {
                val teleportX = newPosition.x match {
                  case x if x < 0 => width
                  case x if x > width => 0
                  case x => x
                }
                val teleportY = newPosition.y match {
                  case y if y < 0 => height
                  case y if y > height => 0
                  case y => y
                }
                moto.teleport(Point(teleportX, teleportY))
              }

              val crash = playersMoto.values
                .filter {
                _ != moto
              }
                .find {
                _.crossMyPaths(newPosition)
              }

              if (crash.isDefined) {
                renderer.displayLoser(player)
                this ! Stop
              }
          }


          renderer.render(playersMoto.values)
        case Stop =>
          timer.stop()
          exit()
      }
    }
  }

  def stop(): Unit = {
    this ! Stop
  }
}
