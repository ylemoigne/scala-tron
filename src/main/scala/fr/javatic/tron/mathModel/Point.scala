package fr.javatic.tron.mathModel

import fr.javatic.tron.Direction

case class Point(val x: Int, val y: Int) {
  def move(direction: Direction.DirectionValue, value: Int) = direction match {
    case Direction.Up => new Point(x, y + value)
    case Direction.Down => new Point(x, y - value)
    case Direction.Left => new Point(x - value, y)
    case Direction.Right => new Point(x + value, y)
  }

  def intersect(v: Vector) = {
    v.intersect(this)
  }
}

object Point {
  private def randomInt(max: Int): Int = (Math.random() * max).asInstanceOf[Int]

  def boundedRandom(maxWidth: Int, maxHeight: Int) = new Point(randomInt(maxWidth), randomInt(maxHeight))
}
