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
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl

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

package org.qslib.quantscale.termstructure.volatility.equityfx

import org.qslib.quantscale.termstructure.VolatilityTermStructure
import org.qslib.quantscale._
import org.qslib.quantscale.time._
import org.scala_tools.time.Imports._
import scala.util.Try

/**
 * ==Local Volatility Term Structure==
 *
 * This trait defines the interface of concrete local-volatility term structures.
 * Volatilities are assumed to be expressed on an annual basis.
 */
trait LocalVolTermStructure extends VolatilityTermStructure {
  refDate: ReferenceDate =>

  // TODO check if this cannot be moved up in the hierarchy
  override val businessDayConvention = Following

  @inline final def localVol(date: LocalDate, underlyingLevel: Real): Try[Volatility] =
    localVol(date, underlyingLevel, false)

  final def localVol(date: LocalDate, underlyingLevel: Real, extrapolate: Boolean): Try[Volatility] =
    localVol(timeFromReference(date), underlyingLevel, extrapolate)

  final def localVol(t: Time, underlyingLevel: Real, extrapolate: Boolean = false): Try[Volatility] =
    Try {
      checkRange(t, extrapolate)
      checkStrike(underlyingLevel, extrapolate)
      localVolImpl(t, underlyingLevel)
    }

  /**
   * This method must be implemented in derived classes to perform the actual volatility
   * calculations. When it is called, range check has already been performed; therefore,
   * it must assume that extrapolation is required.
   */
  protected def localVolImpl(t: Time, strike: Real): Volatility
}
