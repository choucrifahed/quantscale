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
 Copyright (C) 2006 Joseph Wang
 Copyright (C) 2010 Liquidnet Holdings Inc.

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

import org.joda.time.LocalDate
import org.junit.runner.RunWith
import org.qslib.quantscale.Implicits.DecimalToMoney
import org.qslib.quantscale.Implicits.DefaultMoneyConversionConfig.moneyConversionConfig
import org.saddle.Vec
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TimeSeriesSuite extends FunSuite {

  test("Construction test") {
    val ts = TimeSeries(
      new LocalDate(2005, 3, 25) -> 1.2,
      new LocalDate(2005, 3, 29) -> 2.3,
      new LocalDate(2005, 3, 15) -> 0.3).sortedIx

    assert(ts.firstKey.get == new LocalDate(2005, 3, 15), "Date does not match")
    assert(ts.first.get == 0.3, "Value does not match")
  }

  test("Time series interval price test") {
    val dates = Vec(new LocalDate(2005, 3, 25), new LocalDate(2005, 3, 29))

    val open = Vec(1.3 * EUR, 2.3 * EUR)
    val close = Vec(2.3 * EUR, 3.4 * EUR)
    val high = Vec(3.4 * EUR, 3.5 * EUR)
    val low = Vec(3.4 * EUR, 3.2 * EUR)

    val ipts = IntervalPrice.makeSeries(dates, open, close, high, low)

    assert(ipts.first.get == IntervalPrice(1.3 * EUR, 2.3 * EUR, 3.4 * EUR, 3.4 * EUR),
      "Interval price time series not built correctly")
  }
}
