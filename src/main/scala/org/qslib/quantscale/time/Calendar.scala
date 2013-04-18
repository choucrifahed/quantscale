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
 Copyright (C) 2006 Piter Dias

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

import scala.math.BigInt
import org.scala_tools.time.Imports._
import scala.annotation.tailrec
import WeekDay._

/**
 * ==Calendar Trait==
 *
 * This class provides methods for determining whether a date is a
 * business day or a holiday for a given market, and for
 * incrementing/decrementing a date of a given number of business days.
 *
 * A calendar should be defined for specific exchange holiday schedule
 * or for general country holiday schedule.
 */
// FIXME Needs serious testing!
trait Calendar {

  /**
   * @return the name of the calendar.
   * @note This method is used for output and comparison between
   * calendars. It is '''not''' meant to be used for writing
   * switch-on-type code.
   */
  // TODO check if this is necessary with case objects
  def name(): String

  /** @return true if and only if the date is a business day for the given market */
  def isBusinessDay(date: LocalDate): Boolean

  /** @return true if and only if the date is a holiday for the given market */
  def isHoliday(date: LocalDate): Boolean = !isBusinessDay(date)

  /**
   * @return true if and only if the weekday is part of the weekend for the given market
   */
  def isWeekend(weekday: WeekDay.Value): Boolean = weekday == Saturday || weekday == Sunday

  /**
   * @return true if and only if the date is last business day for the month in given market
   */
  def isEndOfMonth(date: LocalDate): Boolean = (date.month != adjust(date + 1.day).month)

  /** @return last business day of the month to which the given date belongs */
  def endOfMonth(date: LocalDate): LocalDate =
    adjust(date.dayOfMonth().withMaximumValue(), Preceding)

  /**
   * Adds a date to the set of holidays for the given calendar.
   * @return the new calendar.
   */
  def addHoliday(date: LocalDate): Calendar

  /**
   * Removes a date from the set of holidays for the given calendar.
   * @return the new calendar.
   */
  def removeHoliday(date: LocalDate): Calendar

  /**
   * Adjusts a non-business day to the appropriate near business day
   * with respect to the given convention.
   */
  def adjust(date: LocalDate, convention: BusinessDayConvention = Following): LocalDate = {
    convention match {
      case Unadjusted => date
      case Following => fSkipHoliday(date)
      case ModifiedFollowing => {
        val fDate = fSkipHoliday(date)
        if (fDate.month != date.month) adjust(date, Preceding) else fDate
      }
      case Preceding => bSkipHoliday(date)
      case ModifiedPreceding => {
        val bDate = bSkipHoliday(date)
        if (bDate.month != date.month) adjust(date, Preceding) else bDate
      }
    }
  }

  /** Advances the given date of the given number of business days. */
  def advance(date: LocalDate, n: Int, unit: TimeUnit,
    convention: BusinessDayConvention = Following,
    endOfMonth: Boolean = false): LocalDate = {

    @tailrec def forward(d: LocalDate, i: Int): LocalDate =
      if (i > 0) forward(fSkipHoliday(d + 1.day), i - 1) else d

    @tailrec def backward(d: LocalDate, i: Int): LocalDate =
      if (i < 0) backward(bSkipHoliday(d - 1.day), i + 1) else d

    if (n == 0) {
      adjust(date, convention)
    } else if (unit == TUDays) {
      if (n > 0) forward(date, n) else backward(date, n)
    } else if (unit == TUWeeks) {
      val result = date + n.weeks
      adjust(result, convention)
    } else {
      val result = if (unit == TUMonths) date + n.months else date + n.years
      if (endOfMonth && isEndOfMonth(date)) {
        this.endOfMonth(result)
      } else {
        adjust(result, convention)
      }
    }
  }

  /** Advances the given date as specified by the given period. */
  def advanceByPeriod(date: LocalDate, period: Period,
    convention: BusinessDayConvention = Following,
    endOfMonth: Boolean = false): LocalDate =
    advance(date, period.getDays(), TUDays, convention, endOfMonth)

  /** Calculates the number of business days between two given dates. */
  def businessDaysBetween(from: LocalDate,
    to: LocalDate,
    includeFirst: Boolean = true,
    includeLast: Boolean = false): Int = {

    @tailrec def businessDaysIter(d: LocalDate, max: LocalDate, wd: Int): Int =
      if (d <= max) businessDaysIter(d + 1.day, max, if (isBusinessDay(d)) wd + 1 else wd)
      else wd

    if (from == to) 0
    else {
      val wd = if (from < to) businessDaysIter(from, to, 0)
      else businessDaysIter(to, from, 0)

      val adjustFirst = if (isBusinessDay(from) && !includeFirst) -1 else 0
      val adjustLast = if (isBusinessDay(to) && !includeLast) -1 else 0
      val result = wd + adjustFirst + adjustLast

      if (from > to) -result else result
    }
  }

  /** @return the holidays between two dates */
  def holidayList(
    from: LocalDate,
    to: LocalDate,
    includeWeekEnds: Boolean = false): Seq[LocalDate] = {

    @tailrec def holidayListIter(d: LocalDate, accu: Seq[LocalDate]): Seq[LocalDate] = if (d <= to) {
      val addDate = isHoliday(d) && (includeWeekEnds || !isWeekend(WeekDay(d)))
      val newAccu = if (addDate) accu :+ d else accu
      holidayListIter(d + 1.day, newAccu)
    } else accu

    require(from < to, "'from' date (" + from + ") must be earlier than 'to' date (" + to + ")")

    holidayListIter(from, Seq())
  }

  @tailrec private[this] def skipHoliday(step: LocalDate => LocalDate)(d: LocalDate): LocalDate =
    if (isHoliday(d)) skipHoliday(step)(step(d)) else d

  private[this] def fSkipHoliday(d: LocalDate): LocalDate = skipHoliday(_ + 1.day)(d)
  private[this] def bSkipHoliday(d: LocalDate): LocalDate = skipHoliday(_ - 1.day)(d)
}

/**
 * This trait provides the means of determining the Easter
 * Monday for a given year, as well as specifying Saturdays
 * and Sundays as weekend days.
 */
trait WesternCalendar extends Calendar {

  final val EasterMonday: Array[Day] = Array(
    98, 90, 103, 95, 114, 106, 91, 111, 102, // 1901-1909
    87, 107, 99, 83, 103, 95, 115, 99, 91, 111, // 1910-1919
    96, 87, 107, 92, 112, 103, 95, 108, 100, 91, // 1920-1929
    111, 96, 88, 107, 92, 112, 104, 88, 108, 100, // 1930-1939
    85, 104, 96, 116, 101, 92, 112, 97, 89, 108, // 1940-1949
    100, 85, 105, 96, 109, 101, 93, 112, 97, 89, // 1950-1959
    109, 93, 113, 105, 90, 109, 101, 86, 106, 97, // 1960-1969
    89, 102, 94, 113, 105, 90, 110, 101, 86, 106, // 1970-1979
    98, 110, 102, 94, 114, 98, 90, 110, 95, 86, // 1980-1989
    106, 91, 111, 102, 94, 107, 99, 90, 103, 95, // 1990-1999
    115, 106, 91, 111, 103, 87, 107, 99, 84, 103, // 2000-2009
    95, 115, 100, 91, 111, 96, 88, 107, 92, 112, // 2010-2019
    104, 95, 108, 100, 92, 111, 96, 88, 108, 92, // 2020-2029
    112, 104, 89, 108, 100, 85, 105, 96, 116, 101, // 2030-2039
    93, 112, 97, 89, 109, 100, 85, 105, 97, 109, // 2040-2049
    101, 93, 113, 97, 89, 109, 94, 113, 105, 90, // 2050-2059
    110, 101, 86, 106, 98, 89, 102, 94, 114, 105, // 2060-2069
    90, 110, 102, 86, 106, 98, 111, 102, 94, 114, // 2070-2079
    99, 90, 110, 95, 87, 106, 91, 111, 103, 94, // 2080-2089
    107, 99, 91, 103, 95, 115, 107, 91, 111, 103, // 2090-2099
    88, 108, 100, 85, 105, 96, 109, 101, 93, 112, // 2100-2109
    97, 89, 109, 93, 113, 105, 90, 109, 101, 86, // 2110-2119
    106, 97, 89, 102, 94, 113, 105, 90, 110, 101, // 2120-2129
    86, 106, 98, 110, 102, 94, 114, 98, 90, 110, // 2130-2139
    95, 86, 106, 91, 111, 102, 94, 107, 99, 90, // 2140-2149
    103, 95, 115, 106, 91, 111, 103, 87, 107, 99, // 2150-2159
    84, 103, 95, 115, 100, 91, 111, 96, 88, 107, // 2160-2169
    92, 112, 104, 95, 108, 100, 92, 111, 96, 88, // 2170-2179
    108, 92, 112, 104, 89, 108, 100, 85, 105, 96, // 2180-2189
    116, 101, 93, 112, 97, 89, 109, 100, 85, 105 // 2190-2199
    )

  /** @return Easter Monday expressed relative to first day of year */
  def easterMonday(year: Year): Day = EasterMonday(year - 1901)
}

/**
 * This trait provides the means of determining the Orthodox
 * Easter Monday for a given year, as well as specifying
 * Saturdays and Sundays as weekend days.
 */
trait OrthodoxCalendar extends Calendar {

  final val EasterMonday: Array[Day] = Array(
    105, 118, 110, 102, 121, 106, 126, 118, 102, // 1901-1909
    122, 114, 99, 118, 110, 95, 115, 106, 126, 111, // 1910-1919
    103, 122, 107, 99, 119, 110, 123, 115, 107, 126, // 1920-1929
    111, 103, 123, 107, 99, 119, 104, 123, 115, 100, // 1930-1939
    120, 111, 96, 116, 108, 127, 112, 104, 124, 115, // 1940-1949
    100, 120, 112, 96, 116, 108, 128, 112, 104, 124, // 1950-1959
    109, 100, 120, 105, 125, 116, 101, 121, 113, 104, // 1960-1969
    117, 109, 101, 120, 105, 125, 117, 101, 121, 113, // 1970-1979
    98, 117, 109, 129, 114, 105, 125, 110, 102, 121, // 1980-1989
    106, 98, 118, 109, 122, 114, 106, 118, 110, 102, // 1990-1999
    122, 106, 126, 118, 103, 122, 114, 99, 119, 110, // 2000-2009
    95, 115, 107, 126, 111, 103, 123, 107, 99, 119, // 2010-2019
    111, 123, 115, 107, 127, 111, 103, 123, 108, 99, // 2020-2029
    119, 104, 124, 115, 100, 120, 112, 96, 116, 108, // 2030-2039
    128, 112, 104, 124, 116, 100, 120, 112, 97, 116, // 2040-2049
    108, 128, 113, 104, 124, 109, 101, 120, 105, 125, // 2050-2059
    117, 101, 121, 113, 105, 117, 109, 101, 121, 105, // 2060-2069
    125, 110, 102, 121, 113, 98, 118, 109, 129, 114, // 2070-2079
    106, 125, 110, 102, 122, 106, 98, 118, 110, 122, // 2080-2089
    114, 99, 119, 110, 102, 115, 107, 126, 118, 103, // 2090-2099
    123, 115, 100, 120, 112, 96, 116, 108, 128, 112, // 2100-2109
    104, 124, 109, 100, 120, 105, 125, 116, 108, 121, // 2110-2119
    113, 104, 124, 109, 101, 120, 105, 125, 117, 101, // 2120-2129
    121, 113, 98, 117, 109, 129, 114, 105, 125, 110, // 2130-2139
    102, 121, 113, 98, 118, 109, 129, 114, 106, 125, // 2140-2149
    110, 102, 122, 106, 126, 118, 103, 122, 114, 99, // 2150-2159
    119, 110, 102, 115, 107, 126, 111, 103, 123, 114, // 2160-2169
    99, 119, 111, 130, 115, 107, 127, 111, 103, 123, // 2170-2179
    108, 99, 119, 104, 124, 115, 100, 120, 112, 103, // 2180-2189
    116, 108, 128, 119, 104, 124, 116, 100, 120, 112 // 2190-2199
    )

  /** @return Easter Monday expressed relative to first day of year */
  def easterMonday(year: Year): Day = EasterMonday(year - 1901)
}
