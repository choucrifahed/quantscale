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
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2001, 2002, 2003 Sadruddin Rejeb
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

package org.qslib.quantscale

import org.qslib.quantscale.pattern.Observer
import org.qslib.quantscale.pattern.Observable
import org.saddle.Vec
import org.saddle.Mat
import org.joda.time.LocalDate

/**
 * Multi-dimensional stochastic process class.
 * This class describes a stochastic process governed by:
 *
 * dx(t)=mu(t,x(t))dt + sigma(t,x(t))dW(t)
 */
// FIXME Find a way to include math formulae in Scaladoc
trait StochasticProcess extends Observer with Observable {

  /** @return the number of dimensions of the stochastic process */
  def size(): Int

  /** @return the number of independent factors of the process */
  def factors(): Int = size()

  /** @return the initial values of the state variables */
  def initialValues(): Vec[Real]

  /** @return the drift part of the equation, i.e. mu(t,x(t)) */
  def drift(t: Time, x: Vec[Real]): Vec[Real]

  /** @return the diffusion part of the equation, i.e. sigma(t,x(t)) */
  def diffusion(t: Time, x: Vec[Real]): Mat[Real]

  /**
   * @return the expectation E(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  def expectation(t0: Time, x0: Vec[Real], dt: Time): Vec[Real]

  /**
   * @return the standard deviation S(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  def stdDeviation(t0: Time, x0: Vec[Real], dt: Time): Mat[Real]

  /**
   * @return the covariance V(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  def covariance(t0: Time, x0: Vec[Real], dt: Time): Mat[Real]

  /**
   * By default, it returns:
   * E(x0, t0, delta t) + S(x0, t0, delta t).delta w
   * where E is the expectation and S the standard deviation.
   *
   * @return the asset value after a time interval delta t according to the
   * given discretization.
   */
  def evolve(t0: Time, x0: Vec[Real], dt: Time, dw: Vec[Real]): Vec[Real]

  /**
   * Applies a change to the asset value. By default, it returns:
   * x + delta x
   */
  def apply(x0: Vec[Real], dx: Vec[Real]): Vec[Real]

  /**
   * @return the time value corresponding to the given date in the reference
   * system of the stochastic process.
   * @note as a number of processes might not need this functionality, a default
   * implementation is given which raises an exception.
   */
  def time(date: LocalDate): Time = throw new UnsupportedOperationException(
    "Date/Time conversion not supported.")

  override def update() {
    notifyObservers()
  }
}

/**
 * ==Multi-Dimensional Stochastic Process==
 *
 * This trait describes a stochastic process governed by:
 * dx(t) = mu(t,x(t)) dt + sigma(t,x(t)) dW(t)
 */
trait StochasticProcessND extends StochasticProcess {

  override final def expectation(t0: Time, x0: Vec[Real], dt: Time): Vec[Real] =
    apply(x0, discretization.drift(this, t0, x0, dt))

  override final def stdDeviation(t0: Time, x0: Vec[Real], dt: Time): Mat[Real] =
    discretization.diffusion(this, t0, x0, dt)

  override final def covariance(t0: Time, x0: Vec[Real], dt: Time): Mat[Real] =
    discretization.covariance(this, t0, x0, dt)

  override final def evolve(t0: Time, x0: Vec[Real], dt: Time, dw: Vec[Real]): Vec[Real] =
    apply(expectation(t0, x0, dt), (stdDeviation(t0, x0, dt) dot dw) col 0)

  override final def apply(x0: Vec[Real], dx: Vec[Real]): Vec[Real] = x0 + dx

  def discretization: Discretization

  /** Multi dimensional discretization of a stochastic process over a given time interval. */
  trait Discretization {
    def drift(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Vec[Real]
    def diffusion(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Mat[Real]
    def covariance(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Mat[Real]
  }
}

/**
 * ==1-Dimensional Stochastic Process==
 *
 * This trait describes a stochastic process governed by:
 * dx(t) = mu(t,x(t)) dt + sigma(t,x(t)) dW(t)
 */
trait StochasticProcess1D extends StochasticProcess {

  /** @return the initial value of the state variable */
  def x0(): Real

  /** @return the drift part of the equation, i.e. mu(t,x(t)) */
  def drift(t: Time, x: Real): Real

  /** @return the diffusion part of the equation, i.e. sigma(t,x(t)) */
  def diffusion(t: Time, x: Real): Real

  /**
   * @return the expectation E(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  final def expectation(t0: Time, x0: Real, dt: Time): Real =
    apply(x0, discretization.drift(this, t0, x0, dt))

  /**
   * @return the standard deviation S(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  final def stdDeviation(t0: Time, x0: Real, dt: Time): Real =
    discretization.diffusion(this, t0, x0, dt)

  /**
   * @return the variance V(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  final def variance(t0: Time, x0: Real, dt: Time): Real =
    discretization.variance(this, t0, x0, dt)

  /**
   * By default, it returns:
   * E(x0, t0, delta t) + S(x0, t0, delta t).delta w
   * where E is the expectation and S the standard deviation.
   *
   * @return the asset value after a time interval delta t according to the
   * given discretization.
   */
  final def evolve(t0: Time, x0: Real, dt: Time, dw: Real): Real =
    apply(expectation(t0, x0, dt), stdDeviation(t0, x0, dt) * dw)

  /**
   * Applies a change to the asset value. By default, it returns:
   * x + delta x
   */
  final def apply(x0: Real, dx: Real): Real = x0 + dx

  override final val size = 1

  override final def initialValues() = Vec(x0())

  override final def drift(t: Time, x: Vec[Real]) = Vec(drift(t, x raw 0))

  override final def diffusion(t: Time, x: Vec[Real]) = Mat(Vec(diffusion(t, x raw 0)))

  override final def expectation(t0: Time, x0: Vec[Real], dt: Time) =
    Vec(expectation(t0, x0 raw 0, dt))

  override final def stdDeviation(t0: Time, x0: Vec[Real], dt: Time) =
    Mat(Vec(stdDeviation(t0, x0 raw 0, dt)))

  override final def covariance(t0: Time, x0: Vec[Real], dt: Time) =
    Mat(Vec(variance(t0, x0 raw 0, dt)))

  override final def evolve(t0: Time, x0: Vec[Real], dt: Time, dw: Vec[Real]) =
    Vec(evolve(t0, x0 raw 0, dt, dw raw 0))

  override final def apply(x0: Vec[Real], dx: Vec[Real]) = Vec(apply(x0 raw 0, dx raw 0))

  def discretization: Discretization

  /** Discretization of a 1-D stochastic process. */
  trait Discretization {
    def drift(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Real
    def diffusion(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Real
    def variance(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Real
  }
}
