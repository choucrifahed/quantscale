package org.qslib.quantscale

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success
import org.qslib.quantscale.pattern.LazyObject
import org.joda.time.DateTime

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
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl

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

/**
 * ==Instrument Trait==
 *
 * This trait defines the interface of concrete instruments which will all be derived from this one.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
trait Instrument[A] extends LazyObject {

  //  private var _engine: Option[PricingEngine[A]] = None // Might want to use multiple ones, why not make it an implicit parameter
  //
  //  def pricingEngine = _engine
  //
  //  def pricingEngine_=(pe: PricingEngine[A]) {
  //    if (None != _engine) unregisterWith(_engine.get)
  //    _engine = Some(pe)
  //    registerWith(_engine.get)
  //    // trigger (lazy) recalculation and notify observers
  //    update()
  //  }

  // FIXME This class is not thread-safe!!!

  /**
   * @return the net present value of the instrument.
   */
  def NPF = calculate() map (_.value)
  def errorEstimate = calculate() map (_.errorEstimate)
  def valuationDate = calculate() map (_.valuationDate)
  def additionalResults = calculate() map (_.additionalResults)

  /**
   * @returns whether the instrument might have value greater than zero.
   */
  def isExpired(): Boolean

  /**
   * When a derived result structure is defined for an
   * instrument, this method should be overridden to read from
   * it. This is mandatory in case a pricing engine is used.
   */
  def fetchResults(): Results

  override protected def calculate() = {
    if (isExpired()) {
      setupExpired()
      calculated = true
      cachedResults
    } else {
      super.calculate
    }
  }

  /**
   * This method must leave the instrument in a consistent
   * state when the expiration condition is met.
   */
  protected def setupExpired() {
    cachedResults = future { Results() }
  }

  /**
   * In case a pricing engine is '''not''' used, this
   * method must be overridden to perform the actual
   * calculations and set any needed results. In case
   * a pricing engine is used, the default implementation
   * can be used.
   */
  protected def performCalculations()(implicit engine: PricingEngine[A]) = engine.calculate()
}


