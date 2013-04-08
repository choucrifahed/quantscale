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

/** Option type enumeration. */
sealed trait OptionType
case object Put extends OptionType
case object Call extends OptionType

/**
 * Base trait of all options.
 * It had to be called OptionInstrument to avoid confusion with Scala's Option trait.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
trait OptionInstrument extends Instrument {
  def payoff(): Payoff
  def exercise(): Exercise
  def optionType(): OptionType
}

trait Greeks {
  def delta(): Option[Real]
  def gamma(): Option[Real]
  def theta(): Option[Real]
  def vega(): Option[Real]
  def rho(): Option[Real]
  def dividendRho(): Option[Real]
}

trait MoreGreeks {
  def itmCashProbability(): Option[Real]
  def deltaForward(): Option[Real]
  def elasticity(): Option[Real]
  def thetaPerDay(): Option[Real]
  def strikeSensitivity(): Option[Real]
}
