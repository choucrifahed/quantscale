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
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl

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

package org.qslib.quantscale.time

import com.github.nscala_time.time.Imports._
import org.qslib.quantscale.Time
import org.joda.time.Days

/**
 * ==Day Counter Trait==
 *
 * It provides methods for determining the length of a time
 * period according to given market convention, both as a number
 * of days and as a year fraction.
 */
trait DayCounter {

  /**
   * Might be overloaded by more complex day counters.
   * @return the number of days between two dates.
   */
  def dayCount(startDate: LocalDate, endDate: LocalDate): Int =
    Days.daysBetween(startDate, endDate).getDays()

  /** @return the period between two dates as a fraction of year. */
  def yearFraction(startDate: LocalDate,
    endDate: LocalDate,
    refPeriodStart: LocalDate = LocalDate.today,
    refPeriodEnd: LocalDate = LocalDate.today): Time
}

/** Actual/360 day count convention, also known as "Act/360", or "A/360". */
case object Actual360 extends DayCounter {
  override def toString() = "Actual/360"

  override def yearFraction(startDate: LocalDate, endDate: LocalDate,
    refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Time =
    dayCount(startDate, endDate) / 360.0
}

/**
 * "Actual/365 (Fixed)" day count convention, also know as
 * "Act/365 (Fixed)", "A/365 (Fixed)", or "A/365F".
 *
 * @note According to ISDA, "Actual/365" (without "Fixed") is
 * an alias for "Actual/Actual (ISDA)" (see ActualActual.)
 * If Actual/365 is not explicitly specified as fixed in an
 * instrument specification, you might want to double-check
 * its meaning.
 */
case object Actual365Fixed extends DayCounter {
  override def toString() = "Actual/360"

  override def yearFraction(startDate: LocalDate, endDate: LocalDate,
    refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Time =
    dayCount(startDate, endDate) / 365.0
}
