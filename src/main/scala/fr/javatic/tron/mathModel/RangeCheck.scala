package fr.javatic.tron.mathModel

class RangeCheck(val v: Double) {
  def in(lowerBound: Double, upperBound: Double, mode: RangeCheckMode.Value) = {
    mode match {
      case RangeCheckMode.Inclusive => v >= lowerBound && v <= upperBound
      case RangeCheckMode.Exclusive => v > lowerBound && v < upperBound
    }
  }

  def notIn(lowerBound: Double, upperBound: Double, mode: RangeCheckMode.Value) = {
    mode match {
      case RangeCheckMode.Inclusive => in(lowerBound, upperBound, RangeCheckMode.Exclusive)
      case RangeCheckMode.Exclusive => in(lowerBound, upperBound, RangeCheckMode.Inclusive)
    }
  }
}
