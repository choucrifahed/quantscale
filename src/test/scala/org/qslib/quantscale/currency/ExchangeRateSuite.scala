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

package org.qslib.quantscale.currency

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.qslib.quantscale._
import org.scala_tools.time.Imports._
import org.qslib.quantscale.Implicits._

@RunWith(classOf[JUnitRunner])
class ExchangeRateSuite extends FunSuite {

  test("Direct exchange rates test") {
    val EURUSD = ExchangeRate(EUR, USD, 1.2042)

    val m1 = 50000.0 * EUR
    val m2 = 100000.0 * USD

    val calculated = (EURUSD exchange m1).get
    val expected = (m1.value * EURUSD.rate) * USD

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected + "\n"
      + "Calculated:      " + calculated)

    val calculated2 = (EURUSD exchange m2).get
    val expected2 = (m2.value / EURUSD.rate) * EUR

    assert(calculated2 ~= expected2, "Wrong result:\n"
      + "Expected:        " + expected2 + "\n"
      + "Calculated:      " + calculated2)
  }

  test("Derived exchange rates test") {
    val EURUSD = ExchangeRate(EUR, USD, 1.2042)
    val EURGBP = ExchangeRate(EUR, GBP, 0.6612)
    val derived = EURUSD :: EURGBP

    val m1 = 50000.0 * GBP
    val m2 = 100000.0 * USD

    val calculated = (derived.get exchange m1).get
    val expected = (m1.value * EURUSD.rate / EURGBP.rate) * USD

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected + "\n"
      + "Calculated:      " + calculated)

    val calculated2 = (derived.get exchange m2).get
    val expected2 = (m2.value * EURGBP.rate / EURUSD.rate) * GBP

    assert(calculated2 ~= expected2, "Wrong result:\n"
      + "Expected:        " + expected2 + "\n"
      + "Calculated:      " + calculated2)
  }

  test("Lookup of direct exchange rates test") {
    ExchangeRateManager.clear

    val EURUSD = ExchangeRate(EUR, USD, 1.1983)
    val USDEUR = ExchangeRate(USD, EUR, 1.0 / 1.2042)

    val date = new LocalDate(2004, 8, 4)
    ExchangeRateManager.add(EURUSD, date)
    ExchangeRateManager.add(USDEUR, date + 1.days)

    val m1 = 50000.0 * EUR
    val m2 = 100000.0 * USD

    val EURUSDLookup = ExchangeRateManager.lookup(EUR, USD, date, ExchangeRateType.Direct).get
    val calculated = (EURUSDLookup exchange m1).get
    val expected = (m1.value * EURUSD.rate) * USD

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected + "\n"
      + "Calculated:      " + calculated)

    val EURUSDLookup2 = ExchangeRateManager.lookup(EUR, USD, date + 1.days, ExchangeRateType.Direct).get
    val calculated2 = (EURUSDLookup2 exchange m1).get
    val expected2 = (m1.value / USDEUR.rate) * USD

    assert(calculated2 ~= expected2, "Wrong result:\n"
      + "Expected:        " + expected2 + "\n"
      + "Calculated:      " + calculated2)

    val USDEURLookup = ExchangeRateManager.lookup(USD, EUR, date, ExchangeRateType.Direct).get
    val calculated3 = (USDEURLookup exchange m2).get
    val expected3 = (m2.value / EURUSD.rate) * EUR

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected3 + "\n"
      + "Calculated:      " + calculated3)

    val USDEURLookup2 = ExchangeRateManager.lookup(USD, EUR, date + 1.days, ExchangeRateType.Direct).get
    val calculated4 = (USDEURLookup2 exchange m2).get
    val expected4 = (m2.value * USDEUR.rate) * EUR

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected4 + "\n"
      + "Calculated:      " + calculated4)
  }

  test("Lookup of triangulated exchange rates test") {
    ExchangeRateManager.clear

    import Europe.ITL
    val EURUSD1 = ExchangeRate(EUR, USD, 1.1983)
    val EURUSD2 = ExchangeRate(EUR, USD, 1.2042)

    val date = new LocalDate(2004, 8, 4)
    ExchangeRateManager.add(EURUSD1, date)
    ExchangeRateManager.add(EURUSD2, date + 1.days)

    val m1 = 50000000.0 * ITL
    val m2 = 100000.0 * USD

    val ITLUSD1 = ExchangeRateManager.lookup(ITL, USD, date).get
    val calculated = (ITLUSD1 exchange m1).get
    val expected = (m1.value * EURUSD1.rate / 1936.27) * USD

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected + "\n"
      + "Calculated:      " + calculated)

    val ITLUSD2 = ExchangeRateManager.lookup(ITL, USD, date + 1.days).get
    val calculated2 = (ITLUSD2 exchange m1).get
    val expected2 = (m1.value * EURUSD2.rate / 1936.27) * USD

    assert(calculated2 ~= expected2, "Wrong result:\n"
      + "Expected:        " + expected2 + "\n"
      + "Calculated:      " + calculated2)

    val USDITL1 = ExchangeRateManager.lookup(USD, ITL, date).get
    val calculated3 = (USDITL1 exchange m2).get
    val expected3 = (m2.value * 1936.27 / EURUSD1.rate) * ITL

    assert(calculated3 ~= expected3, "Wrong result:\n"
      + "Expected:        " + expected3 + "\n"
      + "Calculated:      " + calculated3)

    val USDITL2 = ExchangeRateManager.lookup(USD, ITL, date + 1.days).get
    val calculated4 = (USDITL2 exchange m2).get
    val expected4 = (m2.value * 1936.27 / EURUSD2.rate) * ITL

    assert(calculated4 ~= expected4, "Wrong result:\n"
      + "Expected:        " + expected4 + "\n"
      + "Calculated:      " + calculated4)
  }

  test("Lookup of derived exchange rates test") {
    ExchangeRateManager.clear

    val EURUSD = ExchangeRate(EUR, USD, 1.1983)
    val USDEUR = ExchangeRate(USD, EUR, 1.0 / 1.2042)
    val date = new LocalDate(2004, 8, 4)
    ExchangeRateManager.add(EURUSD, date)
    ExchangeRateManager.add(USDEUR, date + 1.days)

    val GBPEUR = ExchangeRate(GBP, EUR, 1.0 / 0.6596)
    val EURGBP = ExchangeRate(EUR, GBP, 0.6612)
    ExchangeRateManager.add(GBPEUR, date)
    ExchangeRateManager.add(EURGBP, date + 1.days)

    val USDCHF = ExchangeRate(USD, CHF, 1.2847)
    val CHFUSD = ExchangeRate(CHF, USD, 1.0 / 1.2774)
    ExchangeRateManager.add(USDCHF, date)
    ExchangeRateManager.add(CHFUSD, date + 1.days)

    import Europe.SEK
    val SEKCHF = ExchangeRate(SEK, CHF, 0.1674)
    val CHFSEK = ExchangeRate(CHF, SEK, 1.0 / 0.1677)
    ExchangeRateManager.add(SEKCHF, date)
    ExchangeRateManager.add(CHFSEK, date + 1.days)

    val SEKJPY = ExchangeRate(SEK, JPY, 14.5450)
    val JPYSEK = ExchangeRate(JPY, SEK, 1.0 / 14.6110)
    ExchangeRateManager.add(SEKJPY, date)
    ExchangeRateManager.add(JPYSEK, date + 1.days)

    val m1 = 100000.0 * USD
    val m2 = 100000.0 * EUR
    val m3 = 100000.0 * GBP
    val m4 = 100000.0 * CHF
    val m5 = 100000.0 * SEK
    val m6 = 100000.0 * JPY

    // two-rate chain

    val USDSEKLookup = ExchangeRateManager.lookup(USD, SEK, date).get
    val calculated = (USDSEKLookup exchange m1).get
    val expected = (m1.value * USDCHF.rate / SEKCHF.rate) * SEK

    assert(calculated ~= expected, "Wrong result:\n"
      + "Expected:        " + expected + "\n"
      + "Calculated:      " + calculated)

    val SEKUSDLookup = ExchangeRateManager.lookup(SEK, USD, date + 1.days).get
    val calculated2 = (SEKUSDLookup exchange m5).get
    val expected2 = (m5.value * CHFUSD.rate / CHFSEK.rate) * USD

    assert(calculated2 ~= expected2, "Wrong result:\n"
      + "Expected:        " + expected2 + "\n"
      + "Calculated:      " + calculated2)

    // three-rate chain

    val EURSEKLookup = ExchangeRateManager.lookup(EUR, SEK, date).get
    val calculated3 = (EURSEKLookup exchange m2).get
    val expected3 = (m2.value * EURUSD.rate * USDCHF.rate / SEKCHF.rate) * SEK

    assert(calculated3 ~= expected3, "Wrong result:\n"
      + "Expected:        " + expected3 + "\n"
      + "Calculated:      " + calculated3)

    val SEKEURLookup = ExchangeRateManager.lookup(SEK, EUR, date + 1.days).get
    val calculated4 = (SEKEURLookup exchange m5).get
    val expected4 = (m5.value * USDEUR.rate * CHFUSD.rate / CHFSEK.rate) * EUR

    assert(calculated4 ~= expected4, "Wrong result:\n"
      + "Expected:        " + expected4 + "\n"
      + "Calculated:      " + calculated4)

    // four-rate chain

    val EURJPYLookup = ExchangeRateManager.lookup(EUR, JPY, date).get
    val calculated5 = (EURJPYLookup exchange m2).get
    val expected5 = (m2.value * EURUSD.rate * USDCHF.rate * SEKJPY.rate / SEKCHF.rate) * JPY

    assert(calculated5 ~= expected5, "Wrong result:\n"
      + "Expected:        " + expected5 + "\n"
      + "Calculated:      " + calculated5)

    val JPYEURLookup = ExchangeRateManager.lookup(JPY, EUR, date + 1.days).get
    val calculated6 = (JPYEURLookup exchange m6).get
    val expected6 = (m6.value * JPYSEK.rate * USDEUR.rate * CHFUSD.rate / CHFSEK.rate) * EUR

    assert(calculated6 ~= expected6, "Wrong result:\n"
      + "Expected:        " + expected6 + "\n"
      + "Calculated:      " + calculated6)

    // five-rate chain

    val GBPJPYLookup = ExchangeRateManager.lookup(GBP, JPY, date).get
    val calculated7 = (GBPJPYLookup exchange m3).get
    val expected7 = (m3.value * GBPEUR.rate * EURUSD.rate * USDCHF.rate * SEKJPY.rate / SEKCHF.rate) * JPY

    assert(calculated7 ~= expected7, "Wrong result:\n"
      + "Expected:        " + expected7 + "\n"
      + "Calculated:      " + calculated7)

    val JPYGBPLookup = ExchangeRateManager.lookup(JPY, GBP, date + 1.days).get
    val calculated8 = (JPYGBPLookup exchange m6).get
    val expected8 = (m6.value * JPYSEK.rate * USDEUR.rate * CHFUSD.rate * EURGBP.rate / CHFSEK.rate) * GBP

    assert(calculated8 ~= expected8, "Wrong result:\n"
      + "Expected:        " + expected8 + "\n"
      + "Calculated:      " + calculated8)
  }
}
