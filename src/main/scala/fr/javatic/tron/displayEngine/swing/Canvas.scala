package fr.javatic.tron.displayEngine.swing

import scala.swing.Panel
import java.awt.{Dimension, Color, Graphics2D}
import fr.javatic.tron.gameModel.{Player, Moto}
import fr.javatic.tron.displayEngine.Renderer

class Canvas(val width: Int, val height: Int) extends Panel with Renderer {
  opaque = true
  background = Color.WHITE
  focusable = true
  preferredSize = new Dimension(width, height)

  listenTo(keys)

  var motos: Iterable[Moto] = Nil
  var looser: Option[Player] = None

  def render(motos: Iterable[Moto]): Unit = {
    this.motos = motos
    repaint()
  }


  def displayLoser(player: Player): Unit = {
    looser = Some(player)
    repaint()
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    if (looser.isDefined) {
      g.drawString(s"${looser.get.name} has loose !", width / 2, height / 2)
    }

    defineYFromBottomToUp(g)
    motos.foreach(_.draw(g))
  }

  private def defineYFromBottomToUp(g: Graphics2D): Unit = {
    g.scale(1, -1)
    g.translate(0, -height)
  }
}
