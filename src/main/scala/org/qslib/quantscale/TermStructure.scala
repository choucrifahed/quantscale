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

import org.qslib.quantscale.time.DayCounter
import org.qslib.quantscale.time.Calendar
import org.scala_tools.time.Imports._
import org.qslib.quantscale.Implicits._
import scala.util.Try
import scala.util.Success

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
trait TermStructure {
  refDate: ReferenceDate =>

  /** @return the day counter used for date/time conversion. */
  def dayCounter(): DayCounter

  /** Converts a date into time */
  @inline final def timeFromReference(date: LocalDate): Time =
    dayCounter().yearFraction(referenceDate(), date)

  /** @return the latest date for which the curve can return values. */
  def maxDate(): LocalDate

  /** @return the latest time for which the curve can return values. */
  @inline final def maxTime(): Time = timeFromReference(maxDate())

  /** @return the date at which discount = 1.0 and/or variance = 0.0. */
  def referenceDate(): LocalDate = refDate()

  /** @return the calendar used for reference and/or option date calculation. */
  def calendar(): Calendar

  /** Date-range check. */
  protected final def checkRange(date: LocalDate, extrapolate: Boolean) {
    require(referenceDate() <= date, s"date ($date) before reference date (${referenceDate()})")
    require(extrapolate || date <= maxDate(),
      s"date ($date) is past max curve date (${maxDate()})")
  }

  /** Time-range check */
  protected final def checkRange(t: Time, extrapolate: Boolean) {
    require(0.0 <= t, s"negative time ($t) given")
    require(extrapolate || t <= maxTime() || (t ~= maxTime()),
      s"time ($t) is past max curve time (${maxTime()})")
  }
}

sealed trait ReferenceDate {
  def apply(): LocalDate
}

class FixedReferenceDate(date: LocalDate) extends ReferenceDate {
  override def apply() = date
}

class MovingReferenceDate(val settlementDays: Natural) extends ReferenceDate {
  termStructure: TermStructure =>

  override def apply() = {
    val today = Settings.evaluationDate()
    termStructure.calendar.advance(today, settlementDays, time.TUDays)
  }
}
