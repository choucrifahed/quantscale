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
 Copyright (C) 2004, 2005, 2006, 2007 StatPro Italia srl

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

package org.qslib.quantscale

import org.qslib.quantscale.pattern.Observable
import org.qslib.quantscale.pattern.Observer
import org.qslib.quantscale.math.interpolation.Extrapolator
import org.qslib.quantscale.time.DayCounter
import org.qslib.quantscale.time.Calendar
import org.scala_tools.time.Imports._
import org.qslib.quantscale.Implicits._

/**
 * Basic term-structure functionality.
 *
 * There are three ways in which a term structure can keep
 * track of its reference date.  The first is that such date
 * is fixed; the second is that it is determined by advancing
 * the current date of a given number of business days; and
 * the third is that it is based on the reference date of
 * some other structure.
 */
trait TermStructure extends Extrapolator {

  /** @return the day counter used for date/time conversion. */
  def dayCounter(): DayCounter

  /** Converts a date into time */
  @inline
  def timeFromReference(date: LocalDate)(implicit evaluationDate: LocalDate): Time =
    dayCounter().yearFraction(referenceDate(), date)

  /** @return the latest date for which the curve can return values. */
  def maxDate(): LocalDate

  /** @return the latest time for which the curve can return values. */
  @inline
  def maxTime()(implicit evaluationDate: LocalDate): Time = timeFromReference(maxDate())

  /** @return the date at which discount = 1.0 and/or variance = 0.0. */
  def referenceDate()(implicit evaluationDate: LocalDate): LocalDate = calendar().advance(
    evaluationDate, settlementDays(), time.TUDays)

  /** @return the calendar used for reference and/or option date calculation. */
  def calendar(): Calendar

  /** @return the settlementDays used for reference date calculation */
  def settlementDays(): Natural

  /** Date-range check. */
  protected final def checkRange(date: LocalDate, extrapolate: Boolean)(implicit evaluationDate: LocalDate): Boolean =
    referenceDate() <= date && (extrapolate || allowExtrapolation() || date <= maxDate())

  /** Time-range check */
  protected final def checkRange(time: Time, extrapolate: Boolean)(implicit evaluationDate: LocalDate): Boolean =
    0.0 <= time && (extrapolate || allowExtrapolation() || time <= maxTime() || (time ~= maxTime()))
}
