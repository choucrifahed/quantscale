package org.qslib.quantscale

/*
 Copyright (C) 2013 Choucri FAHED

 This source code is release under the BSD License.

 This file is part of QuantScale, a free-software/open-source library
 for financial quantitative analysts and developers - 
 http://github.com/choucrifahed/quantscale

 QuantScale is free software: you can redistribute it and/or modify it
 under the terms of the QuantScale license.  You should have received a
 copy of the license along with this program; if not, please email
 <choucri.fahed@mines-nancy.org>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.

 QuantScale is based on QuantLib. http://quantlib.org/
 When applicable, the original copyright notice follows this notice.
 */

case class Precision(p: Double) extends AnyVal

trait AlmostEqual[E] {
  def ~=(that: E)(implicit p: Precision)
}

object AlmostEqual {
  implicit val precision = Precision(0.00001)

  implicit class AlmostEqualDecimal(val d: Decimal) extends AlmostEqual[Decimal] {
    def ~=(d2: Decimal)(implicit p: Precision) = {
      val diff = (d - d2).abs
      (diff == 0.0) || (if (d == 0.0) diff / d2 <= p.p else diff / d <= p.p)
    }
  }
}
