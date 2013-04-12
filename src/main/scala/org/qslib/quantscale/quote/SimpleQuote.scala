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
 Copyright (C) 2007 Ferdinando Ametrano
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

package org.qslib.quantscale.quote

import org.qslib.quantscale.pattern._
import org.qslib.quantscale._
import scala.concurrent.stm._

/** Market element returning a stored value. */
class SimpleQuote(initialValue: Option[Real] = None) extends Quote[Real] with ObservableDefImpl {
  // FIXME consider moving to Agents or ScalaRX
  private[this] val valueRef: Ref[Option[Real]] = Ref(initialValue)

  def value() = valueRef.single()

  /** @return the difference between the new and old value */
  def setValue(newValue: Real): Real = atomic { implicit txn =>
    val diff = newValue - valueRef().getOrElse(0.0)
    if (diff != 0.0) {
      valueRef() = Some(newValue)
      notifyObservers()
    }
    diff
  }

  def reset() = valueRef.single() = None
}
