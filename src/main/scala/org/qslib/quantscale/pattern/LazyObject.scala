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
 Copyright (C) 2003 RiskMap srl

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

package org.qslib.quantscale.pattern

import scala.util.Try
import org.qslib.quantscale._
import scala.concurrent._
import ExecutionContext.Implicits.global
import rx.Var

/**
 * Trait for on demand calculation and result caching.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
// FIXME Find a more functional way of solving this problem
trait LazyObject extends Observable with Observer {
  type ResultsType <: Results

  protected val calculated: Var[Boolean] = Var(false)
  protected val frozen: Var[Boolean] = Var(false)
  protected val cachedResults: Var[Future[ResultsType]] = Var(future { emptyResults })

  protected val emptyResults: ResultsType

  /**
   * This method forces the recalculation of any results which would otherwise be cached.
   * Explicit invocation of this method is '''not''' necessary if the object registered
   * itself as observer with the structures on which such results depend.
   * It is strongly advised to follow this policy when possible.
   */
  def recalculate(): Future[ResultsType] = {
    val wasFrozen = frozen()
    calculated() = false
    frozen() = false
    try {
      calculate()
    } finally {
      frozen() = wasFrozen
      notifyObservers()
    }
  }

  /**
   * This method constrains the object to return the presently cached results on successive
   * invocations, even if arguments upon which they depend should change.
   */
  def freeze() {
    frozen() = true
  }

  /**
   * This method reverts the effect of the '''''freeze''''' method, thus re-enabling
   * recalculations.
   */
  def unfreeze() {
    // send notifications, just in case we lost any,
    // but only once, i.e. if it was frozen
    if (frozen()) {
      frozen() = false
      notifyObservers()
    }
  }

  /**
   * This method performs all needed calculations by calling the '''''performCalculations'''''
   * method.
   *
   * WARNING: Objects cache the results of the previous calculation. Such results will be
   * returned upon later invocations of '''''calculate'''''. When the results depend
   * on arguments which could change between invocations, the lazy object must register
   * itself as observer of such objects for the calculations to be performed again when
   * they change.
   *
   * WARNING: Should this method be redefined in derived classes, LazyObject.calculate()
   * should be called in the overriding method.
   */
  protected def calculate(): Future[ResultsType] = {
    if (!calculated() && !frozen()) {
      calculated() = true // prevent infinite recursion in case of bootstrapping
      cachedResults() = performCalculations()
    }
    cachedResults()
  }

  /**
   * This method must implement any calculations which must be
   * (re)done in order to calculate the desired results.
   */
  protected def performCalculations(): Future[ResultsType]

  def update() {
    // forwards notifications only the first time
    if (calculated()) {
      // set to false early
      // 1) to prevent infinite recursion
      // 2) otherwise non-lazy observers would be served obsolete
      //    data because of calculated being still true
      calculated() = false;
      // observers don't expect notifications from frozen objects
      if (!frozen()) {
        notifyObservers()
        // exiting notifyObservers() calculated could be
        // already true because of non-lazy observers
      }
    }
  }
}
