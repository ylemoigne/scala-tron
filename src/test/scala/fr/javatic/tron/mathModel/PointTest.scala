package fr.javatic.tron.mathModel

import org.scalatest.FunSpec
import fr.javatic.tron.Direction

class PointTest extends FunSpec {
  val classUnderTest = classOf[Point].getSimpleName

  describe(s"A ${classUnderTest}") {
    val ammount = 10
    val position = Point(0, 0)

    it("sould not mutate on move") {
      val newPosition = position.move(Direction.Right, 0)
      assert(newPosition ne position)
    }

    it(s"should return a new ${classUnderTest} with x increased of ammount when the direction is Right") {
      val newPosition = position.move(Direction.Right, ammount)
      assert(newPosition.x === position.x + ammount)
    }

    it(s"should return a new ${classUnderTest} with x decreased of ammount when the direction is Left") {
      val newPosition = position.move(Direction.Left, ammount)
      assert(newPosition.x === position.x - ammount)
    }

    it(s"should return a new ${classUnderTest} with y increased of ammount when the direction is Up") {
      val newPosition = position.move(Direction.Up, ammount)
      assert(newPosition.y === position.y + ammount)
    }

    it(s"should return a new ${classUnderTest} with y decreased of ammount when the direction is Down") {
      val newPosition = position.move(Direction.Down, ammount)
      assert(newPosition.y === position.y - ammount)
    }
  }
}
