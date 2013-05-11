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
 Copyright (C) 2002, 2003, 2004 Ferdinando Ametrano

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

package org.qslib.quantscale.termstructure.volatility.equityfx

import org.qslib.quantscale._
import org.qslib.quantscale.time._
import scala.util.Try

/**
 * Constant local volatility, no time-strike dependence.
 * This class implements the LocalVolatilityTermStructure
 * interface for a constant local volatility (no time/asset
 * dependence).  Local volatility and Black volatility are the
 * same when volatility is at most time dependent, so this class
 * is basically a proxy for BlackVolatilityTermStructure.
 */
case class LocalConstantVol(
  volatility: Quote[Real],
  referenceDate: ReferenceDate,
  dayCounter: DayCounter,
  calendar: Calendar,
  businessDayConvention: BusinessDayConvention = Following) extends LocalVolTermStructure {

  override val minStrike = Double.MinValue
  override val maxStrike = Double.MaxValue
  override val maxDate = MaxDate

  protected override final def localVolImpl(t: Time, strike: Real): Try[Volatility] =
    Try(volatility().get)
}

object LocalConstantVol {
  def apply(
    volatility: Real,
    referenceDate: ReferenceDate,
    dayCounter: DayCounter,
    calendar: Calendar): LocalConstantVol =
    LocalConstantVol(SimpleQuote(volatility), referenceDate, dayCounter, calendar)
}
