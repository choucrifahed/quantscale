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
 Copyright (C) 2001, 2002, 2003 Sadruddin Rejeb
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2004, 2005, 2006, 2007, 2009 StatPro Italia srl

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

package org.qslib.quantscale.process

import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._
import org.qslib.quantscale.termstructure.YieldTermStructure
import org.qslib.quantscale.termstructure.volatility.equityfx._
import org.qslib.quantscale.time._
import scala.util.Try
import org.joda.time.LocalDate
import org.qslib.quantscale.pattern.ObservableDefImpl
import org.qslib.quantscale.termstructure.yieldp.FlatForward
import org.qslib.quantscale.time.calendar.NullCalendar

/**
 * ==Generalized Black-Scholes Stochastic Process==
 *
 * This trait describes the stochastic process governed by:
 * dS(t, S) = (r(t) - q(t) - sigma(t, S)^2 / 2) dt  + sigma dW(t)
 */
// FIXME Find a way to include math formulae in Scaladoc
trait GeneralizedBlackScholesProcess extends StochasticProcess1D {

  def underlying: Quote[Real]

  def dividendYield(): YieldTermStructure

  def riskFreeRate(): YieldTermStructure

  def blackVolatility(): BlackVolTermStructure

  lazy val localVolatility: LocalVolTermStructure = {
    blackVolatility match {
      case constVol @ BlackConstantVol(_, r, d, c, b) =>
        LocalConstantVol(underlying.map(constVol.blackVol(0.0, _).get), r, d, c, b)
      case volCurve @ BlackVarianceCurve(_, _, _, _, _, _, _, _) =>
        LocalVolCurve(volCurve)
      case _ => LocalVolSurface(blackVolatility, riskFreeRate, dividendYield, underlying)
    }
  }

  override final def x0() = underlying()

  override final def drift(t: Time, x: Real): Try[Real] =
    for {
      sigma <- diffusion(t, x)
      t1 = t + dt
      r <- riskFreeRate.forwardRateTime(t, t1, Continuous, NoFrequency, true)
      d <- dividendYield.forwardRateTime(t, t1, Continuous, NoFrequency, true)
    } yield r() - d() - 0.5 * sigma * sigma

  override final def diffusion(t: Time, x: Real): Try[Real] =
    localVolatility.localVol(t, x, true)

  override final def apply(x0: Real, dx: Real): Real = x0 * Math.exp(dx)

  /*! \warning raises a "not implemented" exception.  It should
                     be rewritten to return the expectation E(S) of
                     the process, not exp(E(log S)).
        */
  // override final def expectation(t0: Time, x0: Real, dt: Time): Try[Real] = ???

  override final def evolve(t0: Time, x0: Real, dt: Time, dw: Real): Try[Real] =
    for {
      d <- discretization.drift(this, t0, x0, dt)
      s <- stdDeviation(t0, x0, dt)
    } yield apply(x0, d + s * dw)

  override final def time(date: LocalDate): Time =
    riskFreeRate.dayCounter().yearFraction(riskFreeRate.referenceDate(), date)

  underlying.registerObserver(this)
}

/**
 * ==Black-Scholes (1973) Stochastic Process==
 * This class describes the stochastic process for a stock given by:
 * dS(t, S) = (r(t) - sigma(t, S)^2 / 2) dt + sigma dW(t)
 */
case class BlackScholesProcess(
  underlying: Quote[Real],
  riskFreeRate: YieldTermStructure,
  blackVolatility: BlackVolTermStructure,
  discretization: Discretization1D = EulerDiscretization) extends GeneralizedBlackScholesProcess with ObservableDefImpl {

  override val dividendYield = FlatForward(0.0,
    MovingReferenceDate(NullCalendar, 0),
    Actual365Fixed,
    NullCalendar)
}

/**
 * Merton (1973) extension to the Black-Scholes stochastic process.
 * This class describes the stochastic process for a stock or
 * stock index paying a continuous dividend yield given by:
 * dS(t, S) = (r(t) - q(t) - sigma(t, S)^2 / 2) dt + sigma dW(t)
 */
case class BlackScholesMertonProcess(
  underlying: Quote[Real],
  dividendYield: YieldTermStructure,
  riskFreeRate: YieldTermStructure,
  blackVolatility: BlackVolTermStructure,
  discretization: Discretization1D = EulerDiscretization) extends GeneralizedBlackScholesProcess with ObservableDefImpl

/**
 * ==Black (1976) Stochastic Process==
 * This class describes the stochastic process for a forward or futures
 * contract given by: dS(t, S) = (sigma(t, S)^2 / 2) dt + sigma dW(t)
 */
case class BlackProcess(
  underlying: Quote[Real],
  riskFreeRate: YieldTermStructure,
  blackVolatility: BlackVolTermStructure,
  discretization: Discretization1D = EulerDiscretization) extends GeneralizedBlackScholesProcess with ObservableDefImpl {

  override val dividendYield = riskFreeRate
}

/**
 * ==Garman-Kohlhagen (1983) Stochastic Process==
 * This class describes the stochastic process for an exchange rate given by:
 * dS(t, S) = (r(t) - r_f(t) - sigma(t, S)^2 / 2) dt + sigma dW(t)
 */
case class GarmanKohlagenProcess(
  underlying: Quote[Real],
  foreignRiskFreeTS: YieldTermStructure,
  domesticRiskFreeTS: YieldTermStructure,
  blackVolatility: BlackVolTermStructure,
  discretization: Discretization1D = EulerDiscretization) extends GeneralizedBlackScholesProcess with ObservableDefImpl {

  override val dividendYield = foreignRiskFreeTS
  override val riskFreeRate = domesticRiskFreeTS
}
