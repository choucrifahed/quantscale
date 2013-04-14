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

package org.qslib.quantscale

/**
 * Implicit values and classes for syntactic sugar.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
object Implicits {

  // Implicit values
  // TODO Move this to a proper config file

  implicit val defaultMoneyConversionConfig = MoneyConversionConfig(AutomatedConversion, EUR)
  implicit val defaultPrecision = math.Precision(0.00001)

  // Implicit classes for syntactic sugar

  /** Shortcut to declare money amounts such as 50.0 * EUR instead of Money(50.0, EUR) */
  implicit class DecimalToMoney(val value: Decimal) extends AnyVal {
    def *(currency: Currency)(implicit mcc: MoneyConversionConfig) = Money(value, currency)(mcc)
    def *(money: Money)(implicit mcc: MoneyConversionConfig) = Money(value * money.value, money.currency)(mcc)
    def /(money: Money)(implicit mcc: MoneyConversionConfig) = Money(value / money.value, money.currency)(mcc)
  }

  /** Shortcut to tell if 2 decimals are close enough given a precision */
  implicit class AlmostEqualDecimal(val d: Decimal) extends AnyVal {
    def ~=(d2: Decimal)(implicit p: math.Precision) = {
      val diff = (d - d2).abs
      (diff == 0.0) || (if (d == 0.0) diff / d2 <= p.p else diff / d <= p.p)
    }
  }
}
