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
 Copyright (C) 2003, 2004, 2005 StatPro Italia srl

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

package org.qslib.quantscale.method.finitedifference

import org.qslib.quantscale._
import org.qslib.quantscale.math._
import org.qslib.quantscale.process.GeneralizedBlackScholesProcess
import scala.util.Try
import scala.util.Success

/** Black-Scholes-Merton differential operator. */
object BSMOperator {

  def apply(size: Int, dx: Real, r: Rate, q: Rate, sigma: Volatility): TridiagonalOperator = {
    val sigma2 = sigma * sigma
    val nu = r - q - sigma2 / 2
    val pd = -(sigma2 / dx - nu) / (2 * dx)
    val pu = -(sigma2 / dx + nu) / (2 * dx)
    val pm = sigma2 / (dx * dx) + r

    Zero(size).withMidRows(pd, pm, pu)
  }

  def apply(grid: TransformedGrid,
    process: GeneralizedBlackScholesProcess,
    residualTime: Time): Try[TridiagonalOperator] = {
    for {
      ul <- Try(process.underlying().get)
      pde <- PdeConstantCoeff(PdeBSM(process), residualTime, ul)
    } yield pde.generateOperator(residualTime, grid, Zero(grid.size))
  }
}

object BSMTermOperator {
  def apply(grid: TransformedGrid,
    process: GeneralizedBlackScholesProcess,
    residualTime: Time = 0.0): TridiagonalOperator =
    PdeOperator(grid, PdeBSM(process), residualTime).apply(Zero(grid.size))
}

// Comes from OperatorFactory in QuantLib
object BSMOperatorFactory {
  def apply(grid: TransformedGrid,
    process: GeneralizedBlackScholesProcess,
    residualTime: Time,
    isTimeDependent: Boolean): Try[TridiagonalOperator] =
    if (isTimeDependent) Success(BSMTermOperator(grid, process, residualTime))
    else BSMOperator(grid, process, residualTime)
}
