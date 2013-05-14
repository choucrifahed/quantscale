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

package org.qslib.quantscale.math.distribution

import org.qslib.quantscale._
import breeze.stats.distributions.Gaussian

/**
 * ==Normal Distribution Function==
 *
 * Unlike QuantLib where it is implemented, this class is only a wrapper around Breeze classes.
 */
case class NormalDistribution(average: Real = 0.0, sigma: Real = 1.0) extends (Real => Real) {
  private[this] val breezeObj = Gaussian(average, sigma)

  /** Given x, it returns its probability in a Gaussian normal distribution.*/
  override def apply(x: Real): Real = breezeObj.pdf(x)
}

case class CumulativeNormalDistribution(average: Real = 0.0, sigma: Real = 1.0)
  extends (Real => Real) {
  private[this] val breezeObj = Gaussian(average, sigma)

  /** Given x it provides an approximation to the integral of the Gaussian normal distribution. */
  override def apply(x: Real): Real = breezeObj.cdf(x)

  /** Given x, it returns its probability in a Gaussian normal distribution.*/
  def derivative(x: Real): Real = breezeObj.pdf(x)
}
