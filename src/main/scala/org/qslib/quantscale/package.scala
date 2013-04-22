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

import org.scala_tools.time.Imports._
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
  type Payoff = Money => Money

  /** Information about a default-protection contract */
  // comes from default.hpp
  sealed trait Side
  case object Buyer extends Side
  case object Seller extends Side

  // comes from position.hpp
  sealed trait Position
  case object Long extends Position
  case object Short extends Position

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
