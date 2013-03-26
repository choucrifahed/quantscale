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
 Copyright (C) 2002, 2003 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
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

package org.qslib.quantscale

import scala.util.Try
import org.joda.time.DateTime
import org.qslib.quantscale.pattern.Observable
import org.qslib.quantscale.pattern.Observer
import scala.concurrent.Future

/**
 * Interface for pricing engines.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
trait PricingEngine[A] extends Observable {

  protected def validate(argument: A): Try[A]

  def calculate(arguments: A*): Future[Results]

  def update() {
    notifyObservers()
  }
}

// TODO consider moving to Money
case class Results(value: Real = 0, errorEstimate: Option[Real] = Some(0), valuationDate: DateTime = new DateTime, additionalResults: Map[String, Any] = Map()) {

  /**
   * @return any additional result returned by the pricing engine.
   */
  def result(tag: String): Option[Any] = try {
    Some(additionalResults(tag))
  } catch {
    case e: NoSuchElementException => None
  }
}
