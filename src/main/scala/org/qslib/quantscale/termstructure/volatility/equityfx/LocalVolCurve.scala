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

import org.qslib.quantscale._
import scala.util.Try

/** Local volatility curve derived from a Black curve. */
// FIXME Find a way to include math formulae in Scaladoc
case class LocalVolCurve(curve: BlackVarianceCurve) extends LocalVolTermStructure {

  override val businessDayConvention = curve.businessDayConvention
  override val referenceDate = curve.referenceDate
  override val calendar = curve.calendar
  override val dayCounter = curve.dayCounter
  override val maxDate = curve.maxDate
  override val minStrike = Double.MinValue
  override val maxStrike = Double.MaxValue

  /**
   * The relation:
   * integral from 0 to T of sigma_L(t)^2 dt = sigma_B^2 T
   * holds, where sigma_L(t) is the local volatility at time t and
   * sigma_B(T) is the Black volatility for maturity T.
   * From the above, the formula
   * sigma_L(t) = sqrt(derivative by t (sigma_B(t)^2 t))
   * can be deduced which is here implemented.
   */
  protected override def localVolImpl(t: Time, strike: Real): Try[Volatility] = {
    val dt = 1.0 / 365.0
    for {
      var1 <- curve.blackVariance(t, strike, true)
      var2 <- curve.blackVariance(t + dt, strike, true)
      derivative = (var2 - var1) / dt
    } yield Math.sqrt(derivative)
  }
}
