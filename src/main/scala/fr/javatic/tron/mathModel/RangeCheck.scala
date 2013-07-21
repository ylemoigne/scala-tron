package fr.javatic.tron.mathModel

class RangeCheck(val v: Double) {
  def in(lowerBound: Double, upperBound: Double) = {
    v >= lowerBound && v <= upperBound
  }
}
