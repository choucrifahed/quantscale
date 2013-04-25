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
 Copyright (C) 2007 Ferdinando Ametrano

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

package org.qslib.quantscale.termstructure

import org.qslib.quantscale._
import org.qslib.quantscale.time._
import org.scala_tools.time.Imports._

/**
 * ==Volatility Term Structure==
 *
 * This trait defines the interface of concrete volatility structures.
 */
trait VolatilityTermStructure extends TermStructure {
  refDate: ReferenceDate =>

  /** @return The business day convention used in tenor to date conversion. */
  def businessDayConvention(): BusinessDayConvention

  /** Handles period/date conversion swaption style. */
  @inline final def optionDateFromTenor(period: Period): LocalDate =
    calendar().advanceByPeriod(referenceDate(), period, businessDayConvention())

  /** @return The minimum strike for which the term structure can return vols. */
  def minStrike(): Rate

  /** @return the maximum strike for which the term structure can return vols. */
  def maxStrike(): Rate

  /** Strike-range check. */
  protected final def checkStrike(strike: Rate, extrapolate: Boolean): Boolean =
    extrapolate || allowExtrapolation() || (strike >= minStrike() && strike <= maxStrike())
}
