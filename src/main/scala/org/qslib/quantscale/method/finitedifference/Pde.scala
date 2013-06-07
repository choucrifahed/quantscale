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
 Copyright (C) 2005 Joseph Wang

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
import org.qslib.quantscale.math.TransformedGrid
import org.qslib.quantscale.math.TridiagonalOperator
import org.saddle.Vec
import scala.util.Try
import scala.util.Success

/** General trait for one dimensional PDEs */
trait PdeSecondOrderParabolic {

  def diffusion(t: Time, x: Real): Try[Real]
  def drift(t: Time, x: Real): Try[Real]
  def discount(t: Time, x: Real): Try[Real]

  def generateOperator(t: Time, tg: TransformedGrid,
    L: TridiagonalOperator): TridiagonalOperator = {

    val gridI = tg.grid.slice(1, tg.grid.length)

    // FIXME dirty hack remove .get
    val sigma = gridI mapValues (diffusion(t, _).get)
    val nu = gridI mapValues (drift(t, _).get)
    val r = gridI mapValues (discount(t, _).get)
    val sigma2 = sigma * sigma

    val pd = -(sigma2 / tg.dxm - nu) / tg.dx
    val pu = -(sigma2 / tg.dxp + nu) / tg.dx
    val pm = sigma2 / (tg.dxm * tg.dxp) + r

    L.withMidRows(pd, pm, pu)
  }
}

case class PdeConstantCoeff(diffusion: Real, drift: Real, discount: Real)
  extends PdeSecondOrderParabolic {

  override def diffusion(t: Time, x: Real) = Success(diffusion)
  override def drift(t: Time, x: Real) = Success(drift)
  override def discount(t: Time, x: Real) = Success(discount)
}

object PdeConstantCoeff {
  def apply(pde: PdeSecondOrderParabolic, t: Time, x: Real): Try[PdeConstantCoeff] = for {
    diffusion <- pde.diffusion(t, x)
    drift <- pde.drift(t, x)
    discount <- pde.discount(t, x)
  } yield PdeConstantCoeff(diffusion, drift, discount)
}
