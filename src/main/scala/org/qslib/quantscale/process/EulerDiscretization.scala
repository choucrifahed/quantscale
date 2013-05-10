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
 Copyright (C) 2004, 2005 StatPro Italia srl

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

package org.qslib.quantscale.process

import org.qslib.quantscale.Discretization1D
import org.qslib.quantscale.DiscretizationND
import org.qslib.quantscale.Real
import org.qslib.quantscale.StochasticProcess
import org.qslib.quantscale.StochasticProcess1D
import org.qslib.quantscale.Time
import org.saddle.Mat
import org.saddle.Vec

/** Euler discretization for stochastic processes. */
// FIXME Find a way to include math formulae in Scaladoc
// FIXME Is there another discretization? If not merge the content of this object in the StochasticProcess trait
case object EulerDiscretization extends Discretization1D with DiscretizationND {

  /** @return An approximation of the drift defined as mu(t0, x0) * Delta t. */
  override def drift(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Vec[Real] =
    process.drift(t0, x0) * dt

  /** @return An approximation of the drift defined as mu(t0, x0) * Delta t. */
  override def drift(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Real =
    process.drift(t0, x0) * dt

  /** @return An approximation of the diffusion defined as sigma(t0, x0) * sqrt(Delta t). */
  override def diffusion(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Mat[Real] =
    process.diffusion(t0, x0) * Math.sqrt(dt)

  /** @return An approximation of the diffusion defined as sigma(t0, x0) * sqrt(Delta t). */
  override def diffusion(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Real =
    process.diffusion(t0, x0) * Math.sqrt(dt)

  /** @return An approximation of the covariance defined as sigma(t0, x0)^2 * Delta t. */
  override def covariance(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Mat[Real] = {
    val sigma = process.diffusion(t0, x0)
    sigma * sigma.transpose * dt
  }

  /** @return An approximation of the variance defined as sigma(t0, x0)^2 * Delta t. */
  override def variance(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Real = {
    val sigma = process.diffusion(t0, x0)
    sigma * sigma * dt
  }
}
