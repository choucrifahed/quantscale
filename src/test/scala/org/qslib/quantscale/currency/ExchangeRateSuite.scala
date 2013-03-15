package org.qslib.quantscale.math

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.qslib.quantscale._
import AlmostEqual._

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

@RunWith(classOf[JUnitRunner])
class ExchangeRateSuite extends FunSuite {

  test("Direct exchange rates test") {
    val eur_usd = ExchangeRate(EUR, USD, 1.2042)

    val m1 = 50000.0 * EUR
    val m2 = 100000.0 * USD

    val conversionType = NoConversion

    val calculated = eur_usd.exchange(m1)
    val expected = (m1.value * eur_usd.rate) * USD

    //    assert(calculated ~= expected, "Original number: " + testCase.x + "\n"
    //        + "Expected:        " + expected + "\n"
    //        + "Calculated:      " + calculated)
    //    
    //    if (!close(calculated,expected)) {
    //        BOOST_FAIL("Wrong result: \n"
    //                   << "    expected:   " << expected << "\n"
    //                   << "    calculated: " << calculated);
    //    }
    //
    //    calculated = eur_usd.exchange(m2);
    //    expected = Money(m2.value()/eur_usd.rate(), EUR);
    //
    //    if (!close(calculated,expected)) {
    //        BOOST_FAIL("Wrong result: \n"
    //                   << "    expected:   " << expected << "\n"
    //                   << "    calculated: " << calculated);
    //    }
  }
}       
 