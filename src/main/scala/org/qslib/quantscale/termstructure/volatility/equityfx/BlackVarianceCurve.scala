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
 Copyright (C) 2002, 2003, 2004 Ferdinando Ametrano
 Copyright (C) 2003 StatPro Italia srl

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

import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._
import org.scala_tools.time.Imports._
import org.qslib.quantscale.time._
import org.qslib.quantscale.math.interpolation._
import org.saddle.Vec
import scala.util.Try

/**
 * Black volatility curve modelled as variance curve.
 *
 * This class calculates time-dependent Black volatilities using as input a vector
 * of (ATM) Black volatilities observed in the market.
 *
 * The calculation is performed interpolating on the variance curve.
 * Linear interpolation is used as default.
 *
 * @see BlackVarianceSurface for strike dependence.
 */
case class BlackVarianceCurve(
  dates: Vec[LocalDate],
  blackVolCurve: Vec[Volatility],
  referenceDate: ReferenceDate,
  dayCounter: DayCounter,
  calendar: Calendar,
  interpolator: Interpolator,
  businessDayConvention: BusinessDayConvention = Following,
  forceMonotoneVariance: Boolean = true) extends BlackVarianceTermStructure {

  require(dates.length == blackVolCurve.length,
    s"Mismatch between date sequence size (${dates.length}) and black vol sequence size (${blackVolCurve.length}")

  // Cannot have dates()==referenceDate, since the value of the vol at dates(0) would be lost.
  // Variance at referenceDate must be zero.
  require(dates.first.get > referenceDate(), s"Cannot have dates(0)(${dates(0)} <= referenceDate ($referenceDate)")
  require(dates.isStrictlyAscending, "Dates must be strictly sorted in an ascending order")

  override val minStrike = Double.MinValue
  override val maxStrike = Double.MaxValue
  override val maxDate = dates.last.get

  private val times = Vec(0.0) concat dates.mapValues(timeFromReference)
  private val variances = Vec(0.0) concat (times.tail(1).zipMap(blackVolCurve)((time, vol) => time * vol * vol))

  require(!forceMonotoneVariance || variances.isAscending, "Variance must be non-decreasing")

  private val varianceCurve = interpolator(times, variances)

  protected override final def blackVarianceImpl(t: Time, strike: Real): Try[Real] = Try {
    if (t <= times.last) {
      varianceCurve(t, true)
    } else {
      // extrapolate with flat vol
      varianceCurve(times.last, true) * t / times.last
    }
  }
}
