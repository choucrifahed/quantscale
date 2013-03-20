package org.qslib

import org.joda.time.LocalDate

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

/**
 * ==Default QuantScale Types & Methods==
 *
 * @author Choucri FAHED
 * @since 1.0
 */
package object quantscale {

  // Brief Custom types

  /** Real number. */
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

  /** Information about a default-protection contract */
  // comes from default.hpp
  object Side extends Enumeration {
    type Side = Value

    val Buyer, Seller = Value
  }

  val MinDate = new LocalDate(0, 1, 1)
  val MaxDate = new LocalDate(9999, 12, 31)

  // Most frequently used currencies
  val CHF = org.qslib.quantscale.currency.Europe.CHF
  val EUR = org.qslib.quantscale.currency.Europe.EUR
  val GBP = org.qslib.quantscale.currency.Europe.GBP
  val JPY = org.qslib.quantscale.currency.Asia.JPY
  val USD = org.qslib.quantscale.currency.America.USD

  // Syntactic sugar
  /** Shortcut to declare money amounts such as 50.0 * EUR instead of Money(50.0, EUR) */
  implicit class DecimalToMoney(val value: Double) extends AnyVal {
    def *(currency: Currency)(implicit mcc: MoneyConversionConfig) = Money(value, currency)(mcc)
    def *(money: Money)(implicit mcc: MoneyConversionConfig) = Money(value * money.value, money.currency)(mcc)
    def /(money: Money)(implicit mcc: MoneyConversionConfig) = Money(value / money.value, money.currency)(mcc)
  }
}
