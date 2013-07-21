package fr.javatic.tron.gameModel

import org.scalatest.FunSpec
import java.awt.Graphics2D
import org.scalatest.mock.EasyMockSugar
import fr.javatic.tron.mathModel.{Point, Vector}
import fr.javatic.tron.Direction

class MotoTest extends FunSpec with EasyMockSugar {
  val classUnderTest = classOf[Moto].getSimpleName

  trait Fixture {
    val position = Point(0, 0)
    val length = 10
    val vector = Vector(position, length, Direction.Right.angle)
    val moto = new Moto(vector)

    val g2d = mock[Graphics2D]
  }

  describe(s"A $classUnderTest") {
    it("sould draw a line from position to position's end point") {
      new Fixture {
        expecting {
          g2d.drawLine(vector.origin.x, vector.origin.y, vector.end.x, vector.end.y)
        }

        whenExecuting(g2d) {
          moto.draw(g2d)
        }
      }
    }
  }

  describe(s"A $classUnderTest forwarded going right") {
    it("should draw a line from position to position's end point with x+1") {
      new Fixture {
        moto.forward()

        expecting {
          g2d.drawLine(vector.origin.x, vector.origin.y, vector.end.x + 1, vector.end.y)
        }

        whenExecuting(g2d) {
          moto.draw(g2d)
        }
      }
    }
  }

  describe(s"A $classUnderTest forwarded going right, then turning to up, then forwarded") {
    it("should draw a line from it's initial position to turn's point position, then another line from turn's point position to end point") {
      new Fixture {
        moto.forward()
        moto.turn(Direction.Up)
        moto.forward()

        expecting {
          g2d.drawLine(vector.origin.x, vector.origin.y, vector.end.x + 1, vector.end.y)
          g2d.drawLine(vector.end.x + 1, vector.end.y, vector.end.x + 1, vector.end.y + 1)
        }

        whenExecuting(g2d) {
          moto.draw(g2d)
        }
      }
    }
  }
}
