package fr.javatic.tron.mathModel

import org.scalatest.FunSpec
import fr.javatic.tron.Direction

class PointTest extends FunSpec {
  val classUnderTest = classOf[Point].getSimpleName

  val ammount = 10
  val position = Point(5, 5)

  describe(s"A $classUnderTest $position") {
    it("sould not mutate on translate") {
      val newPosition = position.translate(Direction.Right.angle, 0)
      assert(newPosition ne position)
    }

    it(s"should return a new $classUnderTest with x increased of ammount when the direction is Right") {
      val newPosition = position.translate(Direction.Right.angle, ammount)
      assert(newPosition.x === position.x + ammount)
    }

    it(s"should return a new $classUnderTest with x decreased of ammount when the direction is Left") {
      val newPosition = position.translate(Direction.Left.angle, ammount)
      assert(newPosition.x === position.x - ammount)
    }

    it(s"should return a new $classUnderTest with y increased of ammount when the direction is Up") {
      val newPosition = position.translate(Direction.Up.angle, ammount)
      assert(newPosition.y === position.y + ammount)
    }

    it(s"should return a new $classUnderTest with y decreased of ammount when the direction is Down") {
      val newPosition = position.translate(Direction.Down.angle, ammount)
      assert(newPosition.y === position.y - ammount)
    }

    {
      val projectionOnX = Point(5, 0)
      it(s"sould return a projection on X equals to $projectionOnX") {
        assert(position.projectionOnX === projectionOnX)
      }
    }

    {
      val projectionOnY = Point(0, 5)
      it(s"sould return a projection on Y equals to $projectionOnY") {
        assert(position.projectionOnY === projectionOnY)
      }
    }
  }

  {
    val p = Point(5, 5)
    val expectedAngle = math.Pi / 4
    describe(s"A $classUnderTest $p") {
      it(s"sould have an angle of $expectedAngle") {
        assert(p.angle === expectedAngle)
      }
    }
  }

  {
    val p = Point(-5, 5)
    val expectedAngle = math.Pi / 4 * 3
    describe(s"A $classUnderTest $p") {
      it(s"sould have an angle of $expectedAngle") {
        assert(p.angle === expectedAngle)
      }
    }
  }

  {
    val p = Point(5, -5)
    val expectedAngle = -math.Pi / 4
    describe(s"A $classUnderTest $p") {
      it(s"sould have an angle of $expectedAngle") {
        assert(p.angle === expectedAngle)
      }
    }
  }

  {
    val p = Point(-5, -5)
    val expectedAngle = -math.Pi / 4 * 3
    describe(s"A $classUnderTest $p") {
      it(s"sould have an angle of $expectedAngle") {
        assert(p.angle === expectedAngle)
      }
    }
  }

  {
    val p = Point(0, -5)
    val expectedAngle = -math.Pi / 2
    describe(s"A $classUnderTest $p") {
      it(s"sould have an angle of $expectedAngle") {
        assert(p.angle === expectedAngle)
      }
    }
  }

  {
    val p = Point(-5, 0)
    val expectedAngle = math.Pi
    describe(s"A $classUnderTest $p") {
      it(s"sould have an angle of $expectedAngle") {
        assert(p.angle === expectedAngle)
      }
    }
  }
}
