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
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

package org.qslib.quantscale.termstructure.yieldp

import org.qslib.quantscale._
import org.qslib.quantscale.termstructure.YieldTermStructure
import org.qslib.quantscale.time._
import scala.util.Try

/** Flat interest-rate curve. */
case class FlatForward(
  forward: Quote[Real],
  referenceDate: ReferenceDate,
  dayCounter: DayCounter,
  calendar: Calendar,
  compounding: Compounding = Continuous,
  frequency: Frequency = Annual) extends YieldTermStructure {

  override val maxDate = MaxDate
  override val jumps = Seq()
  override val jumpDates = Seq()

  protected override def discountImpl(t: Time): Try[DiscountFactor] = Try {
    InterestRate(forward().get, dayCounter, compounding, frequency).discountFactor(t)
  }
}

object FlatForward {
  def apply(
    forward: Real,
    referenceDate: ReferenceDate,
    dayCounter: DayCounter,
    calendar: Calendar): FlatForward =
    FlatForward(SimpleQuote(forward), referenceDate, dayCounter, calendar)

  def apply(
    forward: Real,
    referenceDate: ReferenceDate,
    dayCounter: DayCounter,
    calendar: Calendar,
    compounding: Compounding): FlatForward =
    FlatForward(SimpleQuote(forward), referenceDate, dayCounter, calendar, compounding)

  def apply(
    forward: Real,
    referenceDate: ReferenceDate,
    dayCounter: DayCounter,
    calendar: Calendar,
    compounding: Compounding,
    frequency: Frequency): FlatForward =
    FlatForward(SimpleQuote(forward), referenceDate, dayCounter, calendar, compounding, frequency)
}
