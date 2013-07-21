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

  def turn(newDirection: Direction.Value): Unit = {
    mathModel.Vector(Point(paths.head.end.x, paths.head.end.y), 0, newDirection) +=: paths
  }

  def currentPosition = {
    paths.head.end
  }

  def crossMyPaths(p: Point) = {
    paths.foreach {
      path =>
        val i = path.intersect(p)
        if (i.isDefined) {
          println(s"Crash at ${i} (on path:${path} with point:${p}")
        }
    }
    paths.exists {
      _.intersect(p).isDefined
    }
  }

  def teleport(position: Point): Unit = {
    mathModel.Vector(position, 0, paths.head.direction) +=: paths
  }

  def draw(g: Graphics2D): Unit = {
    paths.foreach {
      _.draw(g)
    }
  }
}
