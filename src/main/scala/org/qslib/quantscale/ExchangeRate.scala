package org.qslib.quantscale

import scala.util.Failure
import scala.util.Success
import scala.util.Try

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
 Copyright (C) 2004 Decillion Pty(Ltd)

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

object ExchangeRateType extends Enumeration {
  type ExchangeRateType = Value

  /** Rate is given directly by the user. */
  val Direct = Value

  /** Rate is derived from exchange rates between other currencies. */
  val Derived = Value
}
import ExchangeRateType._

/**
 * Exchange rate between two currencies.
 * The rate '''r''' is given with the convention that a
 * unit of the source is worth '''r''' units of the target.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
case class ExchangeRate(source: Currency, target: Currency, rate: Decimal, erType: ExchangeRateType = Direct) {
  require(rate != 0.0, "An exchange rate cannot be equal to zero!")

  /** Applies the exchange rate to a cash amount. */
  def exchange(amount: Money): Try[Money] =
    if (amount.currency == source) Success(Money(amount.value * rate, target))
    else if (amount.currency == target) Success(Money(amount.value / rate, source))
    else Failure(new IllegalArgumentException("Exchange rate " + this + " not applicable to currency " + amount.currency + "."))
}

object ExchangeRate {

  /** Chains two exchange rates */
  def chain(r1: ExchangeRate, r2: ExchangeRate): Try[ExchangeRate] =
    if (r1.source == r2.source) Success(ExchangeRate(r1.target, r2.target, r2.rate / r1.rate, Derived))
    else if (r1.source == r2.target) Success(ExchangeRate(r1.target, r2.source, 1.0 / (r1.rate * r2.rate), Derived))
    else if (r1.target == r2.source) Success(ExchangeRate(r1.source, r2.target, r1.rate * r2.rate, Derived))
    else if (r1.target == r2.target) Success(ExchangeRate(r1.source, r2.source, r1.rate / r2.rate, Derived))
    else Failure(new IllegalArgumentException("Exchange rates " + r1 + " and " + r2 + " are not chainable."))
}

