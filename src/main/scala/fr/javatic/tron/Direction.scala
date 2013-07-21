package fr.javatic.tron

object Direction extends Enumeration {
  val Up = Value(0)
  val Down = Value(1)
  val Left = Value(2)
  val Right = Value(3)

  private def randomInt(max: Int): Int = (Math.random() * max).asInstanceOf[Int]

  def random = {
    val rnd = Direction.randomInt(4)
    Direction.values.find(v => v.id == rnd) match {
      case Some(s) => s
      case None => throw new IllegalArgumentException("Can't find value for " + rnd)
    }
  }
}
