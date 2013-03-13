package org.qslib.quantscale

case class Precision(val p: Double) extends AnyVal

trait AlmostEqual[E] {
  def ~=(that: E)(implicit p: Precision)
}

object AlmostEqual {
  implicit val precision = Precision(0.001)

  implicit class AlmostEqualDecimal(d: Decimal) extends AlmostEqual[Decimal] {
    def ~=(d2: Decimal)(implicit p: Precision) = {
      val diff = (d - d2).abs
      (diff == 0.0) || (if (d == 0.0) diff / d2 <= p.p else diff / d <= p.p)
    }
  }

  // FIXME this doesn't work!!!
  implicit class AlmostEqualMoney(m: Money) extends AlmostEqual[Money] {
    def ~=(m2: Money)(implicit p: Precision) = m.value.~=(m2.value)(p)
  }
}
