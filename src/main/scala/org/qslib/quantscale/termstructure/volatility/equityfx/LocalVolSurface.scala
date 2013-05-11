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
 Copyright (C) 2003 Ferdinando Ametrano

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

import org.qslib.quantscale.termstructure.YieldTermStructure
import org.qslib.quantscale.Implicits._
import org.qslib.quantscale._
import scala.util.Try

/**
 * Local volatility surface derived from a Black vol surface.
 * For details about this implementation refer to
 * "Stochastic Volatility and Local Volatility," in
 * "Case Studies and Financial Modelling Course Notes," by
 * Jim Gatheral, Fall Term, 2003
 *
 * see www.math.nyu.edu/fellows_fin_math/gatheral/Lecture1_Fall02.pdf
 */
// FIXME this class is untested, probably unreliable.
case class LocalVolSurface(
  blackTS: BlackVolTermStructure,
  riskFreeTS: YieldTermStructure,
  dividendTS: YieldTermStructure,
  underlying: Quote[Real]) extends LocalVolTermStructure {

  override val businessDayConvention = blackTS.businessDayConvention
  override val referenceDate = blackTS.referenceDate
  override val calendar = blackTS.calendar
  override val dayCounter = blackTS.dayCounter
  override val maxDate = blackTS.maxDate
  override val minStrike = blackTS.minStrike
  override val maxStrike = blackTS.maxStrike

  protected final def localVolImpl(t: Time, strike: Real): Try[Volatility] = Try {

    val dr = riskFreeTS.discount(t, true).get
    val dq = dividendTS.discount(t, true).get
    val forwardValue = underlying().get * dq / dr

    // strike derivatives
    val y = Math.log(strike / forwardValue)
    val dy = if (Math.abs(y) > 0.001) y * 0.0001 else 0.000001
    val strikep = strike * Math.exp(dy)
    val strikem = strike / Math.exp(dy)
    val w = blackTS.blackVariance(t, strike, true).get
    val wp = blackTS.blackVariance(t, strikep, true).get
    val wm = blackTS.blackVariance(t, strikem, true).get
    val dwdy = (wp - wm) / (2.0 * dy)
    val d2wdy2 = (wp - 2.0 * w + wm) / (dy * dy)

    // time derivative
    val dwdt = if (t == 0.0) {
      val drpt = riskFreeTS.discount(t + dt, true).get
      val dqpt = dividendTS.discount(t + dt, true).get
      val strikept = strike * dr * dqpt / (drpt * dq)

      val wpt = blackTS.blackVariance(t + dt, strikept, true).get
      assert(wpt >= w, s"decreasing variance at strike $strike between time $t and time ${t + dt}")

      (wpt - w) / dt
    } else {
      val dt = Implicits.dt.min(t / 2.0)
      val drpt = riskFreeTS.discount(t + dt, true).get
      val drmt = riskFreeTS.discount(t - dt, true).get
      val dqpt = dividendTS.discount(t + dt, true).get
      val dqmt = dividendTS.discount(t - dt, true).get

      val strikept = strike * dr * dqpt / (drpt * dq)
      val strikemt = strike * dr * dqmt / (drmt * dq)

      val wpt = blackTS.blackVariance(t + dt, strikept, true).get
      val wmt = blackTS.blackVariance(t - dt, strikemt, true).get

      assert(wpt >= w, s"decreasing variance at strike $strike between time $t and time ${t + dt}")
      assert(w >= wmt, s"decreasing variance at strike $strike between time ${t - dt} and time $t")

      (wpt - wmt) / (2.0 * dt)
    }

    if (dwdy == 0.0 && d2wdy2 == 0.0) { // avoid /w where w might be 0.0
      Math.sqrt(dwdt)
    } else {
      val den1 = 1.0 - y / w * dwdy
      val den2 = 0.25 * (-0.25 - 1.0 / w + y * y / w / w) * dwdy * dwdy
      val den3 = 0.5 * d2wdy2
      val den = den1 + den2 + den3
      val result = dwdt / den

      assert(result >= 0.0, s"negative local vol^2 at strike $strike and time $t;"
        + "the black vol surface is not smooth enough")

      Math.sqrt(result)
    }
  }
}

object LocalVolSurface {
  def apply(
    blackTS: BlackVolTermStructure,
    riskFreeTS: YieldTermStructure,
    dividendTS: YieldTermStructure,
    underlying: Real): LocalVolSurface =
    LocalVolSurface(blackTS, riskFreeTS, dividendTS, SimpleQuote(underlying))
}
