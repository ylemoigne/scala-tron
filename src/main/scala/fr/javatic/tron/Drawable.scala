package fr.javatic.tron

import java.awt.Graphics2D

abstract trait Drawable {
  def draw(g: Graphics2D): Unit
}
