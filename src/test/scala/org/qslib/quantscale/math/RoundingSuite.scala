package org.qslib.quantscale.math

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.qslib.quantscale._

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
class RoundingSuite extends FunSuite {

  case class RoundingTestCase(x: Decimal,
    precision: Int,
    closest: Decimal,
    up: Decimal,
    down: Decimal,
    floor: Decimal,
    ceiling: Decimal)

  private val testData = List(
    RoundingTestCase(0.86313513, 5, 0.86314, 0.86314, 0.86313, 0.86314, 0.86313),
    RoundingTestCase(0.86313, 5, 0.86313, 0.86313, 0.86313, 0.86313, 0.86313),
    RoundingTestCase(-7.64555346, 1, -7.6, -7.7, -7.6, -7.6, -7.6),
    RoundingTestCase(0.13961605, 2, 0.14, 0.14, 0.13, 0.14, 0.13),
    RoundingTestCase(0.14344179, 4, 0.1434, 0.1435, 0.1434, 0.1434, 0.1434),
    RoundingTestCase(-4.74315016, 2, -4.74, -4.75, -4.74, -4.74, -4.74),
    RoundingTestCase(-7.82772074, 5, -7.82772, -7.82773, -7.82772, -7.82772, -7.82772),
    RoundingTestCase(2.74137947, 3, 2.741, 2.742, 2.741, 2.741, 2.741),
    RoundingTestCase(2.13056714, 1, 2.1, 2.2, 2.1, 2.1, 2.1),
    RoundingTestCase(-1.06228670, 1, -1.1, -1.1, -1.0, -1.0, -1.1),
    RoundingTestCase(8.29234094, 4, 8.2923, 8.2924, 8.2923, 8.2923, 8.2923),
    RoundingTestCase(7.90185598, 2, 7.90, 7.91, 7.90, 7.90, 7.90),
    RoundingTestCase(-0.26738058, 1, -0.3, -0.3, -0.2, -0.2, -0.3),
    RoundingTestCase(1.78128713, 1, 1.8, 1.8, 1.7, 1.8, 1.7),
    RoundingTestCase(4.23537260, 1, 4.2, 4.3, 4.2, 4.2, 4.2),
    RoundingTestCase(3.64369953, 4, 3.6437, 3.6437, 3.6436, 3.6437, 3.6436),
    RoundingTestCase(6.34542470, 2, 6.35, 6.35, 6.34, 6.35, 6.34),
    RoundingTestCase(-0.84754962, 4, -0.8475, -0.8476, -0.8475, -0.8475, -0.8475),
    RoundingTestCase(4.60998652, 1, 4.6, 4.7, 4.6, 4.6, 4.6),
    RoundingTestCase(6.28794223, 3, 6.288, 6.288, 6.287, 6.288, 6.287),
    RoundingTestCase(7.89428221, 2, 7.89, 7.90, 7.89, 7.89, 7.89))

  test("Closest decimal rounding test") {
    testData foreach { testCase =>
      val digits = testCase.precision
      val closest = Rounding(digits, Closest)
      val calculated = closest(testCase.x)
      val expected = testCase.closest
      assert(calculated == expected, "Original number: " + testCase.x + "\n"
        + "Expected:        " + expected + "\n"
        + "Calculated:      " + calculated)
    }
  }

  test("Upward decimal rounding test") {
    testData foreach { testCase =>
      val digits = testCase.precision
      val up = Rounding(digits, Up)
      val calculated = up(testCase.x)
      val expected = testCase.up
      assert(calculated == expected, "Original number: " + testCase.x + "\n"
        + "Expected:        " + expected + "\n"
        + "Calculated:      " + calculated)
    }
  }

  test("Downward decimal rounding test") {
    testData foreach { testCase =>
      val digits = testCase.precision
      val down = Rounding(digits, Down)
      val calculated = down(testCase.x)
      val expected = testCase.down
      assert(calculated == expected, "Original number: " + testCase.x + "\n"
        + "Expected:        " + expected + "\n"
        + "Calculated:      " + calculated)
    }
  }

  test("Floor decimal rounding test") {
    testData foreach { testCase =>
      val digits = testCase.precision
      val floor = Rounding(digits, Floor)
      val calculated = floor(testCase.x)
      val expected = testCase.floor
      assert(calculated == expected, "Original number: " + testCase.x + "\n"
        + "Expected:        " + expected + "\n"
        + "Calculated:      " + calculated)
    }
  }

  test("Ceiling decimal rounding test") {
    testData foreach { testCase =>
      val digits = testCase.precision
      val ceiling = Rounding(digits, Ceiling)
      val calculated = ceiling(testCase.x)
      val expected = testCase.ceiling
      assert(calculated == expected, "Original number: " + testCase.x + "\n"
        + "Expected:        " + expected + "\n"
        + "Calculated:      " + calculated)
    }
  }
}       
 