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
 Copyright (C) 2002, 2003 Ferdinando Ametrano
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl

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

package org.qslib.quantscale.termstructure.volatility.equityfx

import org.qslib.quantscale.termstructure.VolatilityTermStructure
import org.qslib.quantscale._
import org.qslib.quantscale.Implicits.epsilon
import org.qslib.quantscale.time._
import org.scala_tools.time.Imports._
import scala.util.Try
import scala.util.Failure

/**
 * ==Black Volatility Term Structure==
 *
 * This trait defines the interface of concrete Black-volatility term structures.
 * Volatilities are assumed to be expressed on an annual basis.
 */
trait BlackVolTermStructure extends VolatilityTermStructure {

  /** @return spot volatility */
  @inline final def blackVol(maturity: LocalDate, strike: Real): Try[Volatility] =
    blackVol(maturity, strike, false)

  /** @return spot volatility */
  final def blackVol(maturity: LocalDate, strike: Real, extrapolate: Boolean): Try[Volatility] =
    blackVol(timeFromReference(maturity), strike, extrapolate)

  /** @return spot volatility */
  final def blackVol(maturity: Time, strike: Real, extrapolate: Boolean = false): Try[Volatility] =
    Try {
      checkRange(maturity, extrapolate)
      checkStrike(strike, extrapolate)
      return blackVolImpl(maturity, strike)
    }

  /** @return spot variance */
  @inline final def blackVariance(maturity: LocalDate, strike: Real): Try[Real] =
    blackVariance(maturity, strike, false)

  /** @return spot variance */
  final def blackVariance(maturity: LocalDate, strike: Real, extrapolate: Boolean): Try[Real] =
    blackVariance(timeFromReference(maturity), strike, extrapolate)

  /** @return spot variance */
  final def blackVariance(maturity: Time, strike: Real, extrapolate: Boolean = false): Try[Real] =
    Try {
      checkRange(maturity, extrapolate)
      checkStrike(strike, extrapolate)
      return blackVarianceImpl(maturity, strike)
    }

  /** @return forward (at-the-money) volatility */
  @inline final def blackForwardVol(startDate: LocalDate, endDate: LocalDate, strike: Real): Try[Volatility] =
    blackForwardVol(startDate, endDate, strike, false)

  /** @return forward (at-the-money) volatility */
  final def blackForwardVol(startDate: LocalDate, endDate: LocalDate, strike: Real,
    extrapolate: Boolean): Try[Volatility] = Try {
    // (redundant) date-based checks
    require(startDate <= endDate, s"$startDate later than $endDate")
    checkRange(endDate, extrapolate)

    // using the time implementation
    val t1 = timeFromReference(startDate)
    val t2 = timeFromReference(endDate)
    return blackForwardVol(t1, t2, strike, extrapolate)
  }

  /** @return forward (at-the-money) volatility */
  final def blackForwardVol(t1: Time, t2: Time, strike: Real, extrapolate: Boolean = false): Try[Volatility] =
    Try {
      require(t1 <= t2, s"$t1 later than $t2")
      checkRange(t2, extrapolate)
      checkStrike(strike, extrapolate)

      if (t1 == t2) {
        if (t1 == 0.0) {
          blackVarianceImpl(epsilon, strike).map(v => Math.sqrt(v / epsilon)).get
        } else {
          val eps = epsilon min t1
          val var1 = blackVarianceImpl(t1 - eps, strike).get
          val var2 = blackVarianceImpl(t2 + eps, strike).get
          assert(var2 >= var1, "variances must be non-decreasing")
          Math.sqrt((var2 - var1) / (2 * eps))
        }
      } else {
        val var1 = blackVarianceImpl(t1, strike).get
        val var2 = blackVarianceImpl(t2, strike).get
        assert(var2 >= var1, "variances must be non-decreasing")
        Math.sqrt((var2 - var1) / (t2 - t1))
      }
    }

  /** @return forward (at-the-money) variance */
  @inline final def blackForwardVariance(startDate: LocalDate, endDate: LocalDate, strike: Real): Try[Real] =
    blackForwardVariance(startDate, endDate, strike, false)

  /** @return forward (at-the-money) variance */
  final def blackForwardVariance(startDate: LocalDate, endDate: LocalDate, strike: Real,
    extrapolate: Boolean): Try[Real] = Try {
    // (redundant) date-based checks
    require(startDate <= endDate, s"$startDate later than $endDate")
    checkRange(endDate, extrapolate)

    // using the time implementation
    val t1 = timeFromReference(startDate)
    val t2 = timeFromReference(endDate)
    return blackForwardVariance(t1, t2, strike, extrapolate)
  }

  /** @return forward (at-the-money) variance */
  final def blackForwardVariance(t1: Time, t2: Time, strike: Real, extrapolate: Boolean = false): Try[Real] =
    Try {
      require(t1 <= t2, s"$t1 later than $t2")
      checkRange(t2, extrapolate)
      checkStrike(strike, extrapolate);
      val v1 = blackVarianceImpl(t1, strike).get
      val v2 = blackVarianceImpl(t2, strike).get
      assert(v2 >= v1, "variances must be non-decreasing")
      v2 - v1
    }

  /**
   * Black variance calculation.
   * @note When this method is called, range check has already been performed;
   * therefore, it must assume that extrapolation is required.
   */
  protected def blackVarianceImpl(t: Time, strike: Real): Try[Real]

  /**
   * Black volatility calculation.
   * @note When this method is called, range check has already been performed;
   * therefore, it must assume that extrapolation is required.
   */
  protected def blackVolImpl(t: Time, strike: Real): Try[Volatility]
}

/**
 * ==Black Volatility Term Structure==
 *
 * This is an adapter to BlackVolTermStructure allowing the programmer to implement only the
 * blackVolImpl() method in derived classes.
 */
trait BlackVolatilityTermStructure extends BlackVolTermStructure {

  /**
   * Black variance calculation.
   * @note When this method is called, range check has already been performed;
   * therefore, it must assume that extrapolation is required.
   */
  protected override final def blackVarianceImpl(t: Time, strike: Real): Try[Real] =
    blackVolImpl(t, strike) map (vol => vol * vol * t)
}

/**
 * ==Black Variance Term Structure==
 *
 * This is an adapter to BlackVolTermStructure allowing the programmer to implement only the
 * blackVarianceImpl() method in derived classes.
 */
trait BlackVarianceTermStructure extends BlackVolTermStructure {

  /**
   * Black volatility calculation.
   * @note When this method is called, range check has already been performed;
   * therefore, it must assume that extrapolation is required.
   */
  protected override final def blackVolImpl(t: Time, strike: Real): Try[Volatility] = {
    val nonZeroMaturity = if (t == 0.0) epsilon else t
    blackVarianceImpl(nonZeroMaturity, strike) map (
      variance => Math.sqrt(variance / nonZeroMaturity))
  }
}
