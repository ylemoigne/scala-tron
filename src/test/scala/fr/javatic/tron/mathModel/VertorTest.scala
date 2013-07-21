package fr.javatic.tron.mathModel

import org.scalatest.FunSpec
import fr.javatic.tron.{Direction, mathModel}

class VertorTest extends FunSpec {
  val classUnderTest = classOf[Vector].getSimpleName

  describe(s"A ${classUnderTest}") {
    val length = 10
    val vectorUp = mathModel.Vector(Point(0, 0), length, Direction.Up)
    val vectorDown = mathModel.Vector(Point(0, 0), length, Direction.Down)
    val vectorLeft = mathModel.Vector(Point(0, 0), length, Direction.Left)
    val vectorRight = mathModel.Vector(Point(0, 0), length, Direction.Right)

    it("should has his end point equals on x when his direction is up") {
      assert(vectorUp.end.x === vectorUp.origin.x)
    }

    it("should has his end point equals on x when his direction is down") {
      assert(vectorDown.end.x === vectorDown.origin.x)
    }

    it("should has his end point equals on y when his direction is left") {
      assert(vectorLeft.end.y === vectorUp.origin.y)
    }

    it("should has his end point equals on y when his direction is right") {
      assert(vectorRight.end.y === vectorDown.origin.y)
    }

    it("sould has his end point increased of(length) on x when his direction is right") {
      assert(vectorRight.end.x === vectorRight.origin.x + length)
    }

    it("sould has his end point decreased of(length) on x when his direction is left") {
      assert(vectorLeft.end.x === vectorLeft.origin.x - length)
    }

    it("sould has his end point increased of(length) on y when his direction is up") {
      assert(vectorUp.end.y === vectorUp.origin.y + length)
    }

    it("sould has his end point decreased of(length) on y when his direction is down") {
      assert(vectorDown.end.y === vectorDown.origin.y - length)
    }

    {
      val vectorA = Vector(Point(0, 0), 10, Direction.Up)
      val vectorB = Vector(Point(1, 0), 10, Direction.Up)
      it(s"${vectorA.toString} should not intersect a ${classUnderTest} ${vectorB.toString}") {
        assert(vectorA.intersect(vectorB) === None)
      }
    }

    {
      val vectorA = Vector(Point(0, 0), 10, Direction.Up)
      val vectorB = Vector(Point(1, 5), 10, Direction.Right)
      it(s"${vectorA.toString} should not intersect a ${classUnderTest} ${vectorB.toString}") {
        assert(vectorA.intersect(vectorB) === None)
      }
    }

    {
      val vectorA = Vector(Point(0, 0), 10, Direction.Up)
      val vectorB = Vector(Point(11, 5), 10, Direction.Left)
      it(s"${vectorA.toString} should not intersect a ${classUnderTest} ${vectorB.toString}") {
        assert(vectorA.intersect(vectorB) === None)
      }
    }

    {
      val vector = Vector(Point(0, 0), 10, Direction.Up)
      val p = Point(1, 5)
      it(s"${vector.toString} should not intersect a ${p.getClass.getSimpleName} ${p.toString}") {
        assert(vector.intersect(p) === None)
      }
    }

    {
      val vector = Vector(Point(711, 253), 17, Direction.Down)
      val p = Point(711, 210)
      it(s"${vector.toString} should not intersect a ${p.getClass.getSimpleName} ${p.toString}") {
        assert(vector.intersect(p) === None)
      }
    }

    {
      val vectorA = Vector(Point(0, -10), 10, Direction.Up)
      val vectorB = Vector(Point(0, 0), 10, Direction.Up)
      val intersection = Point(0, 0)
      it(s"${vectorA.toString} should intersect a ${classUnderTest} ${vectorB.toString} at ${intersection.toString}") {
        assert(vectorA.intersect(vectorB) === Some(List(intersection)))
      }
    }
  }
}
