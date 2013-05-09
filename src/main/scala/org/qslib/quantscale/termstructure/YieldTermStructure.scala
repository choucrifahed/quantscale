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
 Copyright (C) 2004, 2009 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl

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

import org.qslib.quantscale.TermStructure
import org.saddle.Vec
import org.qslib.quantscale._
import org.qslib.quantscale.Implicits.dt
import org.joda.time.LocalDate
import org.qslib.quantscale.time._
import org.scala_tools.time.Imports._

/**
 * ==Interest-Rate Term Structure==
 *
 * This trait defines the interface of concrete interest rate structures.
 */
trait YieldTermStructure extends TermStructure {
  refDate: ReferenceDate =>

  require(jumps.size == jumpDates.size, "Mismatch between number of jumps (" + jumps.size +
    ") and jump dates (" + jumpDates.size + ").")

  // Inspectors
  def jumps: Seq[Quote[Real]]
  def jumpDates: Seq[LocalDate]

  // TODO keep in mind for implementing classes
  final def defaultJumpDates(): Seq[LocalDate] = {
    val baseYear = referenceDate().year().get()
    (0 to jumps.size) map (year => new LocalDate(year + baseYear, 12, 31))
  }

  final def jumpTimes(): Seq[Time] = jumpDates map (timeFromReference(_))

  /**
   * Time is calculated as a fraction of year from the reference date.
   * @return The discount factor from a given date to the reference date.
   */
  @inline
  final def discountDate(date: LocalDate, extrapolate: Boolean = false): DiscountFactor =
    discount(timeFromReference(date), extrapolate)

  /** @return The discount factor from a given time to the reference date. */
  final def discount(t: Time, extrapolate: Boolean = false): DiscountFactor = {
    def loop(i: Int, jumpEffect: Real): Real = {
      val jumpTime = jumpTimes.apply(i)
      if (jumpTime > 0 && jumpTime < t) loop(i + 1, jumpEffect * jumps(i)().getOrElse(0.0))
      else jumpEffect
    }

    checkRange(t, extrapolate)

    if (jumps.isEmpty) {
      discountImpl(t)
    } else {
      val jumpEffect = loop(0, 1.0)
      jumpEffect * discountImpl(t)
    }
  }

  /**
   * Time is calculated as a fraction of year from the reference date.
   * @return The implied zero-yield rate for a given date.
   * @note The resulting interest rate has the required day counting rule.
   */
  final def zeroRateDate(date: LocalDate,
    resultDayCounter: DayCounter,
    compounding: Compounding,
    frequency: Frequency = Annual,
    extrapolate: Boolean = false): InterestRate = {
    if (date == referenceDate()) {
      val compound = 1.0 / discount(dt, extrapolate)
      // t has been calculated with a possibly different day counter
      // but the difference should not matter for very small times
      InterestRate.impliedRate(compound, resultDayCounter, compounding, frequency, dt)
    } else {
      val compound = 1.0 / discountDate(date, extrapolate)
      InterestRate.impliedRate(compound, resultDayCounter, compounding, frequency, referenceDate(), date)
    }
  }

  /**
   * @return The implied zero-yield rate for a given date.
   * @note The resulting interest rate has the same day-counting rule
   * used by the term structure. The same rule should be used
   * for calculating the passed time t.
   */
  final def zeroRateTime(t: Time,
    compounding: Compounding,
    frequency: Frequency = Annual,
    extrapolate: Boolean = false): InterestRate = {

    val time = if (t == 0.0) dt else t
    val compound = 1.0 / discount(time, extrapolate)
    InterestRate.impliedRate(compound, dayCounter, compounding, frequency, time)
  }

  /**
   * Times are calculated as fractions of year from the reference date.
   * @return The forward interest rate between two dates.
   * @note The resulting interest rate has the required day-counting rule.
   * If both dates are equal the instantaneous forward rate is returned.
   */
  final def forwardRateDate(startDate: LocalDate,
    endDate: LocalDate,
    resultDayCounter: DayCounter,
    compounding: Compounding,
    frequency: Frequency = Annual,
    extrapolate: Boolean = false): InterestRate = {
    if (startDate == endDate) {
      checkRange(startDate, extrapolate)
      val t1 = (timeFromReference(startDate) - dt / 2.0) max 0.0
      val t2 = t1 + dt
      val compound = discount(t1, true) / discount(t2, true)
      // times have been calculated with a possibly different day counter
      // but the difference should not matter for very small times
      InterestRate.impliedRate(compound, resultDayCounter, compounding, frequency, dt)
    } else {
      require(startDate < endDate, s"$startDate later than $endDate")
      val compound = discountDate(startDate, extrapolate) / discountDate(endDate, extrapolate)
      InterestRate.impliedRate(compound, resultDayCounter, compounding, frequency, startDate, endDate)
    }
  }

  /**
   * Times are calculated as fractions of year from the reference date.
   * WARNING: Dates are not adjusted for holidays.
   *
   * @return The forward interest rate between two dates.
   * @note The resulting interest rate has the required day-counting rule.
   * If both dates are equal the instantaneous forward rate is returned.
   */
  @inline
  final def forwardRatePeriod(date: LocalDate,
    period: Period,
    resultDayCounter: DayCounter,
    compounding: Compounding,
    frequency: Frequency = Annual,
    extrapolate: Boolean = false): InterestRate =
    forwardRateDate(date, date + period, resultDayCounter, compounding, frequency, extrapolate)

  /**
   * @return The forward interest rate between two times.
   * @note The resulting interest rate has the the same day-counting rule
   * used by the term structure. The same rule should be used for calculating
   * the passed times startTime and endTime.
   */
  final def forwardRateTime(startTime: Time,
    endTime: Time,
    compounding: Compounding,
    frequency: Frequency = Annual,
    extrapolate: Boolean = false): InterestRate = {

    val (diff, compound) = if (startTime == endTime) {
      checkRange(startTime, extrapolate)
      val t1 = (startTime - dt / 2.0) max 0.0
      val t2 = t1 + dt
      (dt, discount(t1, true) / discount(t2, true))
    } else {
      require(startTime < endTime, s"endTime ($endTime) < t1 ($startTime)")
      (endTime - startTime, discount(startTime, extrapolate) / discount(endTime, extrapolate))
    }

    InterestRate.impliedRate(compound, dayCounter, compounding, frequency, diff)
  }

  /**
   * This method must be implemented in derived classes to
   * perform the actual calculations. When it is called,
   * range check has already been performed; therefore, it
   * must assume that extrapolation is required.
   */
  protected def discountImpl(t: Time): DiscountFactor
}
