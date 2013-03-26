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
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2009 StatPro Italia srl

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

import org.joda.time.LocalDate
import org.qslib.quantscale.pattern.ObservableDefImpl

/**
 * Base trait for cash flows.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
trait CashFlow extends Event {

  /**
   * The amount is not discounted, i.e.,
   * it is the actual amount paid at the cash flow date.
   */
  def amount(): Money
}

/** This cash flow pays a predetermined amount at a given date. */
class SimpleCashFlow(val date: LocalDate, val amount: Money) extends CashFlow with ObservableDefImpl with Equals {

  override def toString = s"SimpleCashFlow(date=$date, amount=$amount)"

  def canEqual(other: Any) = other.isInstanceOf[SimpleCashFlow]

  override def equals(other: Any) = other match {
    case that: SimpleCashFlow => that.canEqual(this) && date == that.date && amount == that.amount
    case _ => false
  }

  override def hashCode() = 41 * (41 + date.hashCode) + amount.hashCode
}

object SimpleCashFlow {
  def apply(date: LocalDate, amount: Money) = new SimpleCashFlow(date, amount)
  def unapply(scf: SimpleCashFlow): Option[(LocalDate, Money)] = Some((scf.date, scf.amount))
}

/** Bond redemption */
case class Redemption(override val date: LocalDate, override val amount: Money)
  extends SimpleCashFlow(date, amount)

/** Amortizing payment */
case class AmortizingPayment(override val date: LocalDate, override val amount: Money)
  extends SimpleCashFlow(date, amount)
