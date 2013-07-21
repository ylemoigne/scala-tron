package fr.javatic.tron

import scala.swing._
import scala.swing.event._
import fr.javatic.tron.gameModel._
import fr.javatic.tron.displayEngine.swing.Canvas
import fr.javatic.tron.gameModel.ChangeDirection
import java.awt.Color

object Tron {
  def main(args: Array[String]) {
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

