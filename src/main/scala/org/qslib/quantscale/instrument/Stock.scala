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

package org.qslib.quantscale.instrument

import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._
import org.qslib.quantscale.pattern._
import org.joda.time.DateTime
import scala.concurrent._
import ExecutionContext.Implicits.global

/** Simple stock class */
// FIXME solve the Handle riddle
class Stock(quote: Quote[Money]) extends Instrument with ObservableDefImpl with ObserverDefImpl {
  override type ResultsType = StockResults
  override val emptyResults = StockResults()
  
  registerWith(quote)

  override def isExpired() = false
  override def performCalculations() = future { StockResults(quote().getOrElse(Money.zero)) }
}

case class StockResults(value: Money = Money.zero) extends Results {
  type ValueType = Money

  override val errorEstimate: Option[Money] = Some(Money.zero)
  override def valuationDate: DateTime = new DateTime()
  override val additionalResults: Map[String, Any] = Map[String, Any]()
}
