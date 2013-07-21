package fr.javatic.tron

object Direction {
  val Up = DirectionValue(0, math.Pi / 2)
  val Down = DirectionValue(1, -(math.Pi) / 2)
  val Left = DirectionValue(2, math.Pi)
  val Right = DirectionValue(3, 0)

  val values = List(Up, Down, Left, Right)

  private def randomInt(max: Int): Int = (Math.random() * max).asInstanceOf[Int]

  def random = {
    val rnd = Direction.randomInt(4)
    val v = Direction.values.find(v => v.id == rnd) match {
      case Some(s) => s
      case None => throw new IllegalArgumentException("Can't find value for " + rnd)
    }
    v.asInstanceOf[DirectionValue]
  }

  case class DirectionValue(val id: Int, val angle: Double)

}
