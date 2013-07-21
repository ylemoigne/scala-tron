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

package fr.javatic.tron

import scala.swing._
import scala.swing.event._
import fr.javatic.tron.gameModel._
import fr.javatic.tron.displayEngine.swing.Canvas
import fr.javatic.tron.gameModel.ChangeDirection
import java.awt.Color

object Tron {
  def main(args: Array[String]): Unit = {
    val arenaWidth = 800
    val arenaHeight = 600
    val canvas = new Canvas(arenaWidth, arenaHeight)

    val player1 = new Player("Player 1", Color.RED)
    val player2 = new Player("Player 2", Color.BLUE)
    val arena = new Arena(arenaWidth, arenaHeight, List(player1, player2), canvas)

    canvas.reactions += {
      case KeyPressed(_, Key.Z, _, _) =>
        arena ! new ChangeDirection(player1, Direction.Up)
      case KeyPressed(_, Key.Q, _, _) =>
        arena ! new ChangeDirection(player1, Direction.Left)
      case KeyPressed(_, Key.S, _, _) =>
        arena ! new ChangeDirection(player1, Direction.Down)
      case KeyPressed(_, Key.D, _, _) =>
        arena ! new ChangeDirection(player1, Direction.Right)
    }

    canvas.reactions += {
      case KeyPressed(_, Key.Numpad8, _, _) =>
        arena ! new ChangeDirection(player2, Direction.Up)
      case KeyPressed(_, Key.Numpad4, _, _) =>
        arena ! new ChangeDirection(player2, Direction.Left)
      case KeyPressed(_, Key.Numpad5, _, _) =>
        arena ! new ChangeDirection(player2, Direction.Down)
      case KeyPressed(_, Key.Numpad6, _, _) =>
        arena ! new ChangeDirection(player2, Direction.Right)
    }

    new MainFrame {
      title = "Tron"
      contents = new BorderPanel {
        layout(canvas) = BorderPanel.Position.Center

        layout(new FlowPanel() {
          contents += new Button("Start") {
            reactions += {
              case _: ActionEvent =>
                canvas.requestFocusInWindow()
                arena.start()
            }
          }

          contents += new Button("Stop") {
            reactions += {
              case _: ActionEvent =>
                arena.stop()
            }
          }
        }) = BorderPanel.Position.South
      }

      visible = true
    }
  }
}

