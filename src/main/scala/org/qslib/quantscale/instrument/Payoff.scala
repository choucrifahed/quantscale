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
 Copyright (C) 2003, 2006 Ferdinando Ametrano
 Copyright (C) 2006 Warren Chou
 Copyright (C) 2006, 2008 StatPro Italia srl
 Copyright (C) 2006 Chiara Fornarola

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

import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._

/** Intermediate trait for put/call payoffs */
trait TypePayoff extends Payoff {
  def optionType(): OptionType
}

// TODO check if this is used
// Maybe change to case object since all methods are implemented
/** Payoff based on a floating strike */
trait FloatingTypePayoff extends TypePayoff {
  override def apply(m: Money): Money =
    throw new UnsupportedOperationException("Floating payoff not handled")
}

/**
 *  Intermediate trait for payoffs based on a fixed strike
 *
 *  @tparam T type of the strike: could be Money or Real...
 */
trait StrikedTypePayoff[T] extends TypePayoff {
  def strike(): T
}

/** Dummy Payoff class */
case object NullPayoff extends Payoff {
  override def apply(m: Money): Money =
    throw new UnsupportedOperationException("Dummy payoff given")
}

/** Plain-vanilla payoff */
case class PlainVanillaPayoff(optionType: OptionType, strike: Money) extends StrikedTypePayoff[Money] {
  override def apply(price: Money): Money = optionType match {
    case Call => (price - strike) max (Money zero strike.currency)
    case Put => (strike - price) max (Money zero strike.currency)
  }
}

/** Payoff with strike expressed as percentage */
case class PercentageStrikePayoff(optionType: OptionType, moneyness: Real) extends StrikedTypePayoff[Real] {
  override val strike = moneyness
  override def apply(price: Money): Money = optionType match {
    case Call => price * ((1.0 - moneyness) max 0.0)
    case Put => price * ((moneyness - 1.0) max 0.0)
  }
}

/**
 * Definitions of Binary path-independent payoffs used below, can be found in
 * M. Rubinstein, E. Reiner:"Unscrambling The Binary Code", Risk, Vol.4 no.9,1991.
 * (see: http://www.in-the-money.com/artandpap/Binary%20Options.doc)
 */

/** Binary asset-or-nothing payoff */
case class AssetOrNothingPayoff(optionType: OptionType, strike: Money)
  extends StrikedTypePayoff[Money] {
  override def apply(price: Money): Money = optionType match {
    case Call => if (price > strike) price else Money zero strike.currency
    case Put => if (strike > price) price else Money zero strike.currency
  }
}

/** Binary cash-or-nothing payoff */
case class CashOrNothingPayoff(optionType: OptionType, strike: Money, cashPayoff: Money)
  extends StrikedTypePayoff[Money] {
  override def apply(price: Money): Money = optionType match {
    case Call => if (price > strike) cashPayoff else Money zero strike.currency
    case Put => if (strike > price) cashPayoff else Money zero strike.currency
  }
}

/**
 * ==Binary Gap Payoff==
 *
 * This payoff is equivalent to being
 * a) long a PlainVanillaPayoff at the first strike (same Call/Put type) and
 * b) short a CashOrNothingPayoff at the first strike (same Call/Put type) with
 * cash payoff equal to the difference between the second and the first strike.
 *
 * WARNING: This payoff can be negative depending on the strikes
 */
case class GapPayoff(optionType: OptionType, strike: Money, secondStrike: Money)
  extends StrikedTypePayoff[Money] {
  override def apply(price: Money): Money = optionType match {
    case Call => if (price >= strike) price - secondStrike else Money zero strike.currency
    case Put => if (strike >= price) secondStrike - price else Money zero strike.currency
  }
}

/**
 * ==Binary Superfund Payoff==
 *
 * Superfund sometimes also called "supershare", which can lead to ambiguity; within QuantLib
 * the terms supershare and superfund are used consistently according to the definitions in
 * Bloomberg OVX function's help pages.
 *
 * This payoff is equivalent to being (1/lowerstrike)
 * a) long (short) an AssetOrNothing Call (Put) at the lower strike and
 * b) short (long) an AssetOrNothing Call (Put) at the higher strike
 */
case class SuperFundPayoff(strike: Money, secondStrike: Money)
  extends StrikedTypePayoff[Money] {
  require(strike.isStrictlyPositive, "Strike (" + strike + ") must be strictly positive.")
  require(secondStrike > strike, "Second strike (" + secondStrike +
    ") must be higher than first strike (" + strike + ").")

  override val optionType = Call
  override def apply(price: Money): Money =
    if (price >= strike && price < secondStrike) price / strike else Money zero strike.currency
}

/** Binary Supershare Payoff */
case class SuperSharePayoff(strike: Money, secondStrike: Money, cashPayoff: Money)
  extends StrikedTypePayoff[Money] {
  require(secondStrike > strike, "Second strike (" + secondStrike +
    ") must be higher than first strike (" + strike + ").")

  override val optionType = Call
  override def apply(price: Money): Money =
    if (price >= strike && price < secondStrike) cashPayoff else Money zero strike.currency
}
