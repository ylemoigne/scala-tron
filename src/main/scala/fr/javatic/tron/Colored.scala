package fr.javatic.tron

import java.awt.{Color, Graphics2D}

trait Colored extends Drawable {
  var color: Color = Color.BLACK

  abstract override def draw(g: Graphics2D): Unit = {
    val previousColor = g.getColor
    g.setColor(color)
    super.draw(g)
    g.setColor(previousColor)
  }
}
