
import fr.javatic.tron.Direction

object Test {
  def randomInt(max: Int): Int = (Math.random() * max).asInstanceOf[Int]

  def main(args: Array[String]) {
    println(Direction.values)
    //Direction.values.foreach( v => println(v.id))
  }
}
