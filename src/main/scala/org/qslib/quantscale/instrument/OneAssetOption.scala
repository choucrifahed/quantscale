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
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2007 StatPro Italia srl

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

package org.qslib.quantscale.instrument

import scala.concurrent.ExecutionContext.Implicits.global
import org.qslib.quantscale._
import org.joda.time.DateTime

/**
 * Option on a single asset.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
trait OneAssetOption extends OptionInstrument {
  type ResultsType = OneAssetOptionResults

  val emptyResults = OneAssetOptionResults()

  override def isExpired = SimpleEvent(exercise.lastDate).hasOccurred()

  // Greeks
  def delta()(implicit engine: PricingEngine) = calculate() map (_.delta)
  def gamma()(implicit engine: PricingEngine) = calculate() map (_.gamma)
  def theta()(implicit engine: PricingEngine) = calculate() map (_.theta)
  def vega()(implicit engine: PricingEngine) = calculate() map (_.vega)
  def rho()(implicit engine: PricingEngine) = calculate() map (_.rho)
  def dividendRho()(implicit engine: PricingEngine) = calculate() map (_.dividendRho)

  // More Greeks
  def itmCashProbability()(implicit engine: PricingEngine) = calculate() map (_.itmCashProbability)
  def deltaForward()(implicit engine: PricingEngine) = calculate() map (_.deltaForward)
  def elasticity()(implicit engine: PricingEngine) = calculate() map (_.elasticity)
  def thetaPerDay()(implicit engine: PricingEngine) = calculate() map (_.thetaPerDay)
  def strikeSensitivity()(implicit engine: PricingEngine) = calculate() map (_.strikeSensitivity)
}

/** Results from single-asset option calculation */
case class OneAssetOptionResults(
  // Results fields
  value: Real = 0.0,
  errorEstimate: Option[Real] = None,
  valuationDate: DateTime = new DateTime(),
  additionalResults: Map[String, Any] = Map[String, Any](),

  // Greeks fields
  delta: Option[Real] = None,
  gamma: Option[Real] = None,
  theta: Option[Real] = None,
  vega: Option[Real] = None,
  rho: Option[Real] = None,
  dividendRho: Option[Real] = None,

  // More Greeks fields
  itmCashProbability: Option[Real] = None,
  deltaForward: Option[Real] = None,
  elasticity: Option[Real] = None,
  thetaPerDay: Option[Real] = None,
  strikeSensitivity: Option[Real] = None) extends Results with Greeks with MoreGreeks
