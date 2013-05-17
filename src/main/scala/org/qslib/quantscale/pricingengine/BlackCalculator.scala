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
 Copyright (C) 2003, 2004, 2005, 2006 Ferdinando Ametrano
 Copyright (C) 2006 StatPro Italia srl

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

package org.qslib.quantscale.pricingengine

import org.qslib.quantscale.instrument.StrikedTypePayoff
import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._
import org.qslib.quantscale.math.distribution.CumulativeNormalDistribution
import org.qslib.quantscale.instrument.PlainVanillaPayoff
import org.qslib.quantscale.instrument.CashOrNothingPayoff
import org.qslib.quantscale.instrument.GapPayoff
import org.qslib.quantscale.instrument.PlainVanillaPayoff
import org.qslib.quantscale.instrument.AssetOrNothingPayoff

/**
 * Black 1976 calculator class.
 */
// FIXME \bug When the variance is null, division by zero occur during
//            the calculation of delta, delta forward, gamma, gamma
//            forward, rho, dividend rho, vega, and strike sensitivity.
case class BlackCalculator(
  payoff: StrikedTypePayoff,
  forward: Real,
  stdDev: Real,
  discount: Real = 1.0) {

  // Initialization code

  private val strike = payoff.strikeValue
  private val payoffType = payoff.optionType
  private val variance = stdDev * stdDev

  require(strike >= 0.0, s"strike ($strike) must be non-negative")
  require(forward > 0.0, s"forward ($forward) must be positive")
  require(stdDev >= 0.0, s"stdDev ($stdDev) must be non-negative")
  require(discount > 0.0, "discount ($discount) must be positive")

  private val (d1: Real, d2: Real, cum_d1: Real, cum_d2: Real, n_d1: Real, n_d2: Real) =
    if (stdDev >= epsilon) {
      if (strike ~= 0.0) (Double.MaxValue, Double.MaxValue, 1.0, 1.0, 0.0, 0.0)
      else {
        val f = CumulativeNormalDistribution()
        (Math.log(forward / strike) / stdDev + 0.5 * stdDev,
          d1 - stdDev,
          f(d1),
          f(d2),
          f.derivative(d1),
          f.derivative(d2))
      }
    } else {
      if (forward ~= strike) (0.0, 0.0, 0.5, 0.5, M_SQRT_2 * M_1_SQRTPI, M_SQRT_2 * M_1_SQRTPI)
      else if (forward > strike) (Double.MaxValue, Double.MaxValue, 1.0, 1.0, 0.0, 0.0)
      else (Double.MinValue, Double.MinValue, 0.0, 0.0, 0.0, 0.0)
    }

  private val m: BlackCalculator.More = BlackCalculator.more(this)

  // Public API

  def value(): Real = discount * (forward * m.alpha + m.x * m.beta)

  /** @return Sensitivity to change in the underlying spot price. */
  def delta(spot: Real): Real = {
    require(spot > 0.0, s"Positive spot value required: $spot not allowed")

    val dforwardDs = forward / spot
    val temp = stdDev * spot
    val dalphaDs = m.dalphaDd1 / temp
    val dbetaDs = m.dbetaDd2 / temp
    val temp2 = dalphaDs * forward + alpha * dforwardDs + dbetaDs * m.x + beta * m.dxDs

    discount * temp2
  }

  /** @return Sensitivity to change in the underlying forward price. */
  def deltaForward(): Real = {
    val temp = stdDev * forward
    val dalphaDforward = m.dalphaDd1 / temp
    val dbetaDforward = m.dbetaDd2 / temp
    val temp2 = dalphaDforward * forward + alpha + dbetaDforward * m.x // DXDforward = 0.0

    discount * temp2
  }

  /**
   * @return Sensitivity in percent to a percent change in the
   * underlying spot price.
   */
  def elasticity(spot: Real): Real = {
    val del = delta(spot)
    if (value > epsilon) del / value * spot
    else if (Math.abs(del) < epsilon) 0.0
    else if (del > 0.0) Double.MaxValue
    else Double.MinValue
  }

  /**
   * @return Sensitivity in percent to a percent change in the
   * underlying forward price.
   */
  def elasticityForward(): Real = {
    val del = deltaForward
    if (value > epsilon) del / value * forward
    else if (Math.abs(del) < epsilon) 0.0
    else if (del > 0.0) Double.MaxValue
    else Double.MinValue
  }

  /**
   * @return Second order derivative with respect to change in the
   * underlying spot price.
   */
  def gamma(spot: Real): Real = {
    require(spot > 0.0, s"Positive spot value required: $spot not allowed")

    val dforwardDs = forward / spot

    val temp = stdDev * spot
    val dalphaDs = m.dalphaDd1 / temp
    val dbetaDs = m.dbetaDd2 / temp

    val d2alphaDs2 = -dalphaDs / spot * (1 + d1 / stdDev)
    val d2betaDs2 = -dbetaDs / spot * (1 + d2 / stdDev)

    val temp2 = d2alphaDs2 * forward + 2.0 * dalphaDs * dforwardDs +
      d2betaDs2 * m.x + 2.0 * dbetaDs * m.dxDs

    return discount * temp2
  }

  /**
   * @return Second order derivative with respect to change in the
   * underlying forward price.
   */
  def gammaForward(): Real = {
    val temp = stdDev * forward
    val dalphaDforward = m.dalphaDd1 / temp
    val dbetaDforward = m.dbetaDd2 / temp

    val d2alphaDforward2 = -dalphaDforward / forward * (1 + d1 / stdDev)
    val d2betaDforward2 = -dbetaDforward / forward * (1 + d2 / stdDev)

    val temp2 = d2alphaDforward2 * forward + 2.0 * dalphaDforward +
      d2betaDforward2 * m.x // dXDforward = 0.0

    discount * temp2
  }

  /** @return Sensitivity to time to maturity. */
  def theta(spot: Real, maturity: Time): Real = {
    require(maturity >= 0.0, s"maturity ($maturity) must be non-negative")
    if (maturity ~= 0.0) 0.0
    else -(Math.log(discount) * value +
      Math.log(forward / spot) * spot * delta(spot) +
      0.5 * variance * spot * spot * gamma(spot)) / maturity
  }

  /**
   * @return Sensitivity to time to maturity per day,
   * assuming 365 day per year.
   */
  @inline def thetaPerDay(spot: Real, maturity: Time): Real =
    theta(spot, maturity) / 365.0

  /** @return Sensitivity to volatility. */
  def vega(maturity: Time): Real = {
    require(maturity >= 0.0, "negative maturity not allowed")

    val temp = Math.log(strike / forward) / variance
    // actually DalphaDsigma / SQRT(T)
    val dalphaDsigma = m.dalphaDd1 * (temp + 0.5)
    val dbetaDsigma = m.dbetaDd2 * (temp - 0.5)

    val temp2 = dalphaDsigma * forward + dbetaDsigma * m.x

    discount * Math.sqrt(maturity) * temp2
  }

  /** @return Sensitivity to discounting rate. */
  def rho(maturity: Time): Real = {
    require(maturity >= 0.0, "negative maturity not allowed")

    // actually DalphaDr / T
    val dalphaDr = m.dalphaDd1 / stdDev
    val dbetaDr = m.dbetaDd2 / stdDev
    val temp = dalphaDr * forward + alpha * forward + dbetaDr * m.x

    maturity * (discount * temp - value)
  }

  /** @return Sensitivity to dividend/growth rate. */
  def dividendRho(maturity: Time): Real = {
    require(maturity >= 0.0, "negative maturity not allowed")

    // actually DalphaDq / T
    val dalphaDq = -m.dalphaDd1 / stdDev
    val dbetaDq = -m.dbetaDd2 / stdDev

    val temp = dalphaDq * forward - alpha * forward + dbetaDq * m.x

    return maturity * discount * temp
  }

  /**
   * Probability of being in the money in the bond martingale
   * measure, i.e. N(d2).
   * It is a risk-neutral probability, not the real world one.
   */
  @inline def itmCashProbability(): Real = cum_d2

  /**
   * Probability of being in the money in the asset martingale
   * measure, i.e. N(d1).
   * It is a risk-neutral probability, not the real world one.
   */
  @inline def itmAssetProbability(): Real = cum_d1

  /** @return Sensitivity to strike. */
  def strikeSensitivity(): Real = {
    val temp = stdDev * strike
    val dalphaDstrike = -m.dalphaDd1 / temp
    val dbetaDstrike = -m.dbetaDd2 / temp

    val temp2 = dalphaDstrike * forward + dbetaDstrike * m.x + beta * m.dxDstrike

    discount * temp2
  }

  @inline def alpha(): Real = m.alpha
  @inline def beta(): Real = m.beta
}

object BlackCalculator {

  def apply(
    optionType: OptionType,
    strike: Money,
    forward: Real,
    stdDev: Real): BlackCalculator =
    BlackCalculator(optionType, strike, forward, stdDev, 1.0)

  def apply(
    optionType: OptionType,
    strike: Money,
    forward: Real,
    stdDev: Real,
    discount: Real): BlackCalculator =
    BlackCalculator(PlainVanillaPayoff(optionType, strike), forward, stdDev, discount)

  private def more(c: BlackCalculator): More = c.payoff match {
    case _: CashOrNothingPayoff => new MoreCashOrNothingPayoff(c)
    case _: AssetOrNothingPayoff => new MoreAssetOrNothingPayoff(c)
    case _: GapPayoff => new MoreGapPayoff(c)
    case _: PlainVanillaPayoff => new More(c)
    case _ => throw new IllegalArgumentException("Unsupported payoff type: " + c.payoff)
  }

  private sealed class More(c: BlackCalculator) {
    val x = c.strike
    val dxDstrike = 1.0

    // the following one will probably disappear as soon as
    // super-share will be properly handled
    val dxDs = 0.0

    val (alpha, dalphaDd1, beta, dbetaDd2) = c.payoffType match {
      case Call => (c.cum_d1, c.n_d1, -c.cum_d2, -c.n_d2)
      case Put => (-1.0 + c.cum_d1, c.n_d1, 1.0 - c.cum_d2, -c.n_d2)
    }
  }

  private class MoreCashOrNothingPayoff(c: BlackCalculator) extends More(c) {
    override val alpha, dalphaDd1, dxDstrike = 0.0
    override val x = c.payoff.asInstanceOf[CashOrNothingPayoff].cashPayoff.value

    override val (beta, dbetaDd2) = c.payoffType match {
      case Call => (c.cum_d2, c.n_d2)
      case Put => (1.0 - c.cum_d2, -c.n_d2)
    }
  }

  private class MoreAssetOrNothingPayoff(c: BlackCalculator) extends More(c) {
    override val beta, dbetaDd2 = 0.0

    override val (alpha, dalphaDd1) = c.payoffType match {
      case Call => (c.cum_d1, c.n_d1)
      case Put => (1.0 - c.cum_d1, -c.n_d1)
    }
  }

  private class MoreGapPayoff(c: BlackCalculator) extends More(c) {
    override val x = c.payoff.asInstanceOf[GapPayoff].secondStrike.value
    override val dxDstrike = 0.0
  }
}
