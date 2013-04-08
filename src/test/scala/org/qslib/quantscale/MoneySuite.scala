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
 Copyright (C) 2004 StatPro Italia srl

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

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.qslib.quantscale.currency.ExchangeRateManager
import org.qslib.quantscale.math.Rounding
import Implicits.{DecimalToMoney, AlmostEqualDecimal, defaultPrecision}

@RunWith(classOf[JUnitRunner])
class MoneySuite extends FunSuite {

  test("Money arithmetic without conversions test") {
    implicit val mcc = MoneyConversionConfig(NoConversion, EUR)
    val m1 = 50000.0 * EUR
    val m2 = 100000.0 * EUR
    val m3 = 500000.0 * EUR

    val calculated = m1 * 3.0 + 2.5 * m2 - m3 / 5.0
    val expected = (m1.value * 3.0 + 2.5 * m2.value - m3.value / 5.0) * EUR

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected + "\n"
      + "Calculated:      " + calculated)
  }

  test("Money arithmetic with conversion to base currency test") {
    implicit val mcc = MoneyConversionConfig(BaseCurrencyConversion, EUR)

    ExchangeRateManager.clear()
    val EURUSD = ExchangeRate(EUR, USD, 1.2042)
    val EURGBP = ExchangeRate(EUR, GBP, 0.6612)
    ExchangeRateManager add EURUSD
    ExchangeRateManager add EURGBP

    val m1 = 50000.0 * GBP
    val m2 = 100000.0 * EUR
    val m3 = 500000.0 * USD

    val calculated = m1 * 3.0 + 2.5 * m2 - m3 / 5.0

    val round = EUR.rounding
    val x = round(m1.value * 3.0 / EURGBP.rate) + 2.5 * m2.value - round(m3.value / (5.0 * EURUSD.rate))
    val expected = x * EUR

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected + "\n"
      + "Calculated:      " + calculated)
  }

  test("Money arithmetic with automated conversion test") {
    implicit val mcc = MoneyConversionConfig(AutomatedConversion, EUR)

    val m1 = 50000.0 * GBP
    val m2 = 100000.0 * EUR
    val m3 = 500000.0 * USD

    ExchangeRateManager.clear()
    val EURUSD = ExchangeRate(EUR, USD, 1.2042)
    val EURGBP = ExchangeRate(EUR, GBP, 0.6612)
    ExchangeRateManager add EURUSD
    ExchangeRateManager add EURGBP

    val calculated = (m1 * 3.0 + 2.5 * m2) - m3 / 5.0

    val round = m1.currency.rounding
    val x = m1.value * 3.0 + round(2.5 * m2.value * EURGBP.rate) -
      round((m3.value / 5.0) * EURGBP.rate / EURUSD.rate)
    val expected = x * GBP

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected + "\n"
      + "Calculated:      " + calculated)
  }
}
