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
 Copyright (C) 2003, 2004, 2005 StatPro Italia srl

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

package org.qslib

import com.github.nscala_time.time.Imports._
import org.saddle.Series

/**
 * ==Default QuantScale Types & Methods==
 *
 * @author Choucri FAHED
 * @since 1.0
 */
package object quantscale {

  // Brief Custom types

  /** Positive integer number. */
  type Natural = Int

  /** Real number. */
  // TODO consider using Spire numerics (Saddle integration issue!)
  type Real = Double

  /** Decimal number. */
  type Decimal = Double

  /** Continuous quantity with 1-year units. */
  type Time = Double

  /** Discount factor between dates. */
  type DiscountFactor = Double

  /** Interest rates. */
  type Rate = Double

  /** Spreads on interest rates. */
  type Spread = Double

  /** Volatility. */
  type Volatility = Double

  /** A leg is simply a sequence of cash-flows */
  type Leg = Seq[CashFlow]

  /**
   * ==Container for Historical Data==
   *
   * This class acts as a generic repository for a set of historical data.
   * Any single datum can be accessed through its date, while sets of
   * consecutive data can be accessed through iterators.
   */
  type TimeSeries[T] = Series[LocalDate, T]

  /** Base type for option payoffs. */
  type Payoff = Real => Real

  /** Information about a default-protection contract */
  // Comes from default.hpp
  sealed trait Side
  case object Buyer extends Side
  case object Seller extends Side

  // Comes from position.hpp
  sealed trait Position
  case object Long extends Position
  case object Short extends Position

  // Math constants (more precise than java.lang.Math)
  // Comes from mathconstants.hpp
  val M_E = 2.71828182845904523536
  val M_LOG2E = 1.44269504088896340736
  val M_LOG10E = 0.434294481903251827651
  val M_IVLN10 = 0.434294481903251827651
  val M_LN2 = 0.693147180559945309417
  val M_LOG2_E = 0.693147180559945309417
  val M_LN10 = 2.30258509299404568402
  val M_PI = 3.141592653589793238462643383280
  val M_TWOPI = (M_PI * 2.0)
  val M_PI_2 = 1.57079632679489661923
  val M_PI_4 = 0.785398163397448309616
  val M_3PI_4 = 2.3561944901923448370E0
  val M_SQRTPI = 1.77245385090551602792981
  val M_1_PI = 0.318309886183790671538
  val M_2_PI = 0.636619772367581343076
  val M_1_SQRTPI = 0.564189583547756286948
  val M_2_SQRTPI = 1.12837916709551257390
  val M_SQRT2 = 1.41421356237309504880
  val M_SQRT_2 = 0.7071067811865475244008443621048490392848359376887
  val M_SQRT1_2 = 0.7071067811865475244008443621048490392848359376887
  val M_LN2LO = 1.9082149292705877000E-10
  val M_LN2HI = 6.9314718036912381649E-1
  val M_SQRT3 = 1.73205080756887719000
  val M_INVLN2 = 1.4426950408889633870E0

  val MinDate = new LocalDate(0, 1, 1)
  val MaxDate = new LocalDate(9999, 12, 31)

  // Most frequently used currencies
  val CHF = org.qslib.quantscale.currency.Europe.CHF
  val EUR = org.qslib.quantscale.currency.Europe.EUR
  val GBP = org.qslib.quantscale.currency.Europe.GBP
  val JPY = org.qslib.quantscale.currency.Asia.JPY
  val USD = org.qslib.quantscale.currency.America.USD

  implicit object LocalDateOrdering extends Ordering[LocalDate] {
    def compare(x: LocalDate, y: LocalDate) = x compareTo y
  }
}
