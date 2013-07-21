package fr.javatic.tron.mathModel

import java.awt.Graphics2D
import fr.javatic.tron.{Drawable, Direction}
import scala.util.Try

case class Vector(val origin: Point, val length: Int, val direction: Direction.Value) extends Drawable {
  val end: Point = direction match {
    case Direction.Up => Point(origin.x, origin.y + length)
    case Direction.Down => Point(origin.x, origin.y - length)
    case Direction.Left => Point(origin.x - length, origin.y)
    case Direction.Right => Point(origin.x + length, origin.y)
  }

  def draw(g: Graphics2D) = {
    g.drawLine(origin.x, origin.y, end.x, end.y)
  }

  // Paul Bourke algorithm : http://paulbourke.net/geometry/pointlineplane/
  // Plus fix from http://stackoverflow.com/a/2255848/1016547 for coincident line and 1D intersection
  // Plus my own modification, because if it's a 1D overlap, i want the overlap point from v1.end to v2.origin
  def intersect(that: Vector): Option[List[Point]] = {
    if (this.isPoint) {
      toPoint.get.intersect(that)
    } else if (that.isPoint) {
      intersect(that.toPoint.get)
    } else {
      intersectVector(that)
    }
  }

  def intersectVector(that: Vector): Option[List[Point]] = {
    implicit def doubleToRangeCheck(v: Double) = new RangeCheck(v)

    val p1 = origin
    val p2 = end
    val p3 = that.origin
    val p4 = that.end

    val denominator = (p4.y - p3.y) * (p2.x - p1.x) - (p4.x - p3.x) * (p2.y - p1.y)
    val uaNumerator = (p4.x - p3.x) * (p1.y - p3.y) - (p4.y - p3.y) * (p1.x - p3.x)
    val ubNumerator = (p2.x - p1.x) * (p1.x - p3.y) - (p2.y - p1.y) * (p1.x - p3.x)

    val parrallelLine = denominator == 0
    val coincidentLine = parrallelLine && (uaNumerator == 0) && (ubNumerator == 0)

    if (coincidentLine) {
      return oneDimensionIntersect(that);
    }

    if (parrallelLine) {
      return None
    }

    val ua = uaNumerator.toDouble / denominator
    if (!(ua in(0, 1))) {
      return None
    }

    val ub = ubNumerator.toDouble / denominator
    if (!(ub in(0, 1))) {
      return None
    }

    val ix = p1.x + ua * (p2.x - p1.x)
    val iy = p1.y + ua * (p2.y - p1.y)

    Some(List(Point(ix.toInt, iy.toInt)))
  }

  private def oneDimensionIntersect(b: Vector): Option[List[Point]] = {
    val a1 = end
    val a2 = origin
    val b1 = b.origin
    val b2 = b.end

    val denomx = a2.x - a1.x
    val denomy = a2.y - a1.y

    val ub = if (denomx.abs > denomy.abs)
               ((b1.x - a1.x) / denomx, (b2.x - a1.x) / denomx)
             else
               ((b1.y - a1.y) / denomy, (b2.y - a1.y) / denomy)

    val interval = overlapIntervals(ub._1, ub._2)

    if (interval.isEmpty) {
      None
    } else {
      Some(interval map {
        (f) =>
          val x = a2.x * f + a1.x * (1.0f - f)
          val y = a2.y * f + a1.y * (1.0f - f)
          Point(x.toInt, y.toInt)
      })
    }
  }

  private def overlapIntervals(ub1: Int, ub2: Int): List[Int] = {
    val l = ub1 min ub2
    val r = ub1 max ub2

    val a = 0 max l
    val b = 1 min r

    if (a > b) // no intersection
      return Nil;
    else if (a == b)
           return List(a);
    else // if (A < B)
      return List(a, b);
  }

  def intersect(p: Point): Option[List[Point]] = {
    if (isPoint) {
      if (toPoint.get == p) {
        return Some(List(p))
      } else {
        return None
      }
    }
    // formula here:
    //http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
    // where x0,y0 = p
    //       x1,y1 = q0
    //       x2,y2 = q1
    val dx21 = end.x - origin.x
    val dy21 = end.y - origin.y
    val dx10 = origin.x - p.x
    val dy10 = origin.y - p.y
    val num = (dx21 * dy10 - dx10 * dy21).abs / length.toDouble


    // TODO mais quel hoerreur, voir les crossproduct et dotproduct, renommer et refactoriser tout ça
    val pointIsOnLine = num == 0
    if (pointIsOnLine) {
      val dotproduct = (p.x - origin.x) * (end.x - origin.x) + (p.y - origin.y) * (end.y - origin.y)
      if (dotproduct < 0) {
        return None
      } else {
        val squaredlengthba = length * length
        if (dotproduct > squaredlengthba) {
          return None
        }
      }
      return Some(List(p))
    } else {
      return None
    }
  }

  def isPoint = {
    length == 0
  }

  def toPoint: Try[Point] = {
    Try {
      if (isPoint) {
        origin
      } else {
        throw new IllegalStateException("Origin != End, so this is not a Point")
      }
    }
  }
}
