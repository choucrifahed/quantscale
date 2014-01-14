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
 Copyright (C) 2004, 2005, 2006, 2007 Ferdinando Ametrano

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
import org.qslib.quantscale.time._
import com.github.nscala_time.time.Imports._

sealed trait Compounding

/** 1+rt */
case object Simple extends Compounding

/** (1+r)^t */
case object Compounded extends Compounding

/** e^(rt) */
case object Continuous extends Compounding

/** Simple up to the first period then Compounded */
case object SimpleThenCompounded extends Compounding

/**
 * ==Concrete Interest Rate Class==
 *
 * This class encapsulate the interest rate compounding algebra.
 * It manages day-counting conventions, compounding conventions,
 * conversion between different conventions, discount/compound factor
 * calculations, and implied/equivalent rate calculations.
 */
case class InterestRate(rate: Rate, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency) {

  // Check that compounding is coherent with the frequency
  compounding match {
    case Compounded | SimpleThenCompounded => require(frequency != Once && frequency != NoFrequency,
      s"Frequency ($frequency) not allowed with compounding ($compounding) for interest rates.")
    case _ => // Do nothing
  }

  def apply(): Rate = rate

  /**
   * @return discount factor implied by the rate compounded at time t.
   * @note Time must be measured using InterestRate's own day counter.
   */
  def discountFactor(t: Time): DiscountFactor = 1.0 / compoundFactor(t)

  /** @return discount factor implied by the rate compounded between two dates. */
  def discountFactor(startDate: LocalDate,
    endDate: LocalDate,
    refStart: LocalDate = LocalDate.now,
    refEnd: LocalDate = LocalDate.now): DiscountFactor = {
    require(startDate <= endDate, s"startDate ($startDate) later than endDate ($endDate)")
    val t = dayCounter.yearFraction(startDate, endDate, refStart, refEnd)
    discountFactor(t)
  }

  /**
   * @return the compound (a.k.a capitalization) factor implied by the rate compounded at time t.
   * @note Time must be measured using InterestRate's own day counter.
   */
  def compoundFactor(t: Time): Real = {
    require(t >= 0.0, "Negative time not allowed")
    compounding match {
      case Simple => 1.0 + rate * t
      case Compounded => Math.pow(1.0 + rate / frequency(), frequency() * t)
      case Continuous => Math.exp(rate * t)
      case SimpleThenCompounded =>
        if (t <= 1.0 / frequency()) 1.0 + rate * t
        else Math.pow(1.0 + rate / frequency(), frequency() * t)
    }
  }

  /**
   * @return the compound (a.k.a capitalization) factor implied by the rate compounded
   * between two dates.
   */
  def compoundFactor(startDate: LocalDate,
    endDate: LocalDate,
    refStart: LocalDate = LocalDate.now,
    refEnd: LocalDate = LocalDate.now): Real = {
    require(startDate <= endDate, s"startDate ($startDate) later than endDate ($endDate)")
    val t = dayCounter.yearFraction(startDate, endDate, refStart, refEnd)
    compoundFactor(t)
  }

  /**
   * @return equivalent interest rate for a compounding period t.
   * The resulting InterestRate shares the same implicit
   * day-counting rule of the original InterestRate instance.
   *
   * @note Time must be measured using the InterestRate's own day counter.
   */
  def equivalentRate(comp: Compounding,
    freq: Frequency,
    t: Time): InterestRate =
    InterestRate.impliedRate(compoundFactor(t), dayCounter, comp, freq, t)

  /**
   * @return equivalent rate for a compounding period between two dates.
   * The resulting rate is calculated taking the required day-counting rule into account.
   */
  def equivalentRate(resultDC: DayCounter,
    comp: Compounding,
    freq: Frequency,
    startDate: LocalDate,
    endDate: LocalDate,
    refStart: LocalDate = LocalDate.now,
    refEnd: LocalDate = LocalDate.now): InterestRate = {
    require(startDate <= endDate, s"startDate ($startDate) later than endDate ($endDate)")
    val t1 = dayCounter.yearFraction(startDate, endDate, refStart, refEnd)
    val t2 = resultDC.yearFraction(startDate, endDate, refStart, refEnd)
    InterestRate.impliedRate(compoundFactor(t1), resultDC, comp, freq, t2)
  }
}

object InterestRate {

  /**
   * @return implied interest rate for a given compound factor at a given time.
   * The resulting InterestRate has the day-counter provided as input.
   * @note Time must be measured using the day-counter provided as input.
   */
  def impliedRate(compound: Real,
    resultDC: DayCounter,
    comp: Compounding,
    freq: Frequency,
    t: Time): InterestRate = {

    require(compound > 0.0, s"Positive compound ($compound) factor required.")

    val r = if (compound == 1.0) {
      require(t >= 0.0, s"non negative time ($t) required")
      0.0
    } else {
      require(t > 0.0, s"positive time ($t) required")
      comp match {
        case Simple => (compound - 1.0) / t
        case Compounded => (Math.pow(compound, 1.0 / (freq() * t)) - 1.0) * freq()
        case Continuous => Math.log(compound) / t
        case SimpleThenCompounded =>
          if (t <= 1.0 / freq()) (compound - 1.0) / t
          else (Math.pow(compound, 1.0 / (freq() * t)) - 1.0) * freq()
      }
    }

    InterestRate(r, resultDC, comp, freq)
  }

  /**
   * @return implied rate for a given compound factor between two dates.
   * The resulting rate is calculated taking the required day-counting rule into account.
   */
  def impliedRate(compound: Real,
    resultDC: DayCounter,
    comp: Compounding,
    freq: Frequency,
    startDate: LocalDate,
    endDate: LocalDate,
    refStart: LocalDate = LocalDate.now,
    refEnd: LocalDate = LocalDate.now): InterestRate = {
    require(startDate <= endDate, s"startDate ($startDate) later than endDate ($endDate)")
    val t = resultDC.yearFraction(startDate, endDate, refStart, refEnd)
    impliedRate(compound, resultDC, comp, freq, t)
  }
}

