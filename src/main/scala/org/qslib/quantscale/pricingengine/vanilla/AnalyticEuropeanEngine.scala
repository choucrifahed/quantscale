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
 Copyright (C) 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2007 StatPro Italia srl

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

package org.qslib.quantscale.pricingengine.vanilla

import org.qslib.quantscale._
import org.qslib.quantscale.process.GeneralizedBlackScholesProcess
import scala.concurrent._
import ExecutionContext.Implicits.global
import org.qslib.quantscale.instrument.OneAssetOption
import org.qslib.quantscale.instrument.StrikedTypePayoff
import org.qslib.quantscale.pricingengine.BlackCalculator
import org.qslib.quantscale.instrument.OneAssetOptionResults

/**
 * Pricing engine for European vanilla options using analytical formulae.
 */
/*
 * Test:
 * - the correctness of the returned value is tested by
 * reproducing results available in literature.
 * - the correctness of the returned greeks is tested by
 * reproducing results available in literature.
 * - the correctness of the returned greeks is tested by
 * reproducing numerical derivatives.
 * - the correctness of the returned implied volatility is tested
 * by using it for reproducing the target value.
 * - the implied-volatility calculation is tested by checking
 * that it does not modify the option.
 * - the correctness of the returned value in case of
 * cash-or-nothing digital payoff is tested by reproducing
 * results available in literature.
 * - the correctness of the returned value in case of
 * asset-or-nothing digital payoff is tested by reproducing
 * results available in literature.
 * - the correctness of the returned value in case of gap digital
 * payoff is tested by reproducing results available in
 * literature.
 * - the correctness of the returned greeks in case of
 * cash-or-nothing digital payoff is tested by reproducing
 * numerical derivatives.
 */
case class AnalyticEuropeanEngine(process: GeneralizedBlackScholesProcess) extends PricingEngine {
  override type InstrumentType = OneAssetOption

  def calculate(instrument: InstrumentType): Future[instrument.ResultsType] = future {
    require(instrument.exercise.isInstanceOf[EuropeanExercise], "not an European option")
    require(instrument.payoff.isInstanceOf[StrikedTypePayoff], "non-striked payoff given")

    val exercise = instrument.exercise.asInstanceOf[EuropeanExercise]
    val payoff = instrument.payoff.asInstanceOf[StrikedTypePayoff]

    val variance = process.blackVolatility.blackVariance(exercise.lastDate, payoff.strikeValue).get

    val dividendDiscount = process.dividendYield.discountDate(exercise.lastDate).get
    val riskFreeDiscount = process.riskFreeRate.discountDate(exercise.lastDate).get
    val spot = process.underlying().get
    require(spot > 0.0, "negative or null underlying given")
    val forwardPrice = spot * dividendDiscount / riskFreeDiscount

    val rfdc = process.riskFreeRate.dayCounter
    val divdc = process.dividendYield.dayCounter
    val voldc = process.blackVolatility.dayCounter

    val trfdc = rfdc.yearFraction(process.riskFreeRate.referenceDate(), exercise.lastDate)
    val tdivdc = divdc.yearFraction(process.dividendYield.referenceDate(), exercise.lastDate)
    val tvoldc = voldc.yearFraction(process.blackVolatility.referenceDate(), exercise.lastDate)

    val black = BlackCalculator(payoff, forwardPrice, Math.sqrt(variance), riskFreeDiscount)

    OneAssetOptionResults(
      value = black.value,
      delta = Some(black.delta(spot)),
      deltaForward = Some(black.deltaForward),
      elasticity = Some(black.elasticity(spot)),
      gamma = Some(black.gamma(spot)),
      rho = Some(black.rho(trfdc)),
      dividendRho = Some(black.dividendRho(tdivdc)),
      vega = Some(black.vega(tvoldc)),
      theta = try { Some(black.theta(spot, tvoldc)) }
      catch { case _: Exception => None },
      thetaPerDay = try { Some(black.thetaPerDay(spot, tvoldc)) }
      catch { case _: Exception => None },
      strikeSensitivity = Some(black.strikeSensitivity),
      itmCashProbability = Some(black.itmCashProbability))
  }
}
