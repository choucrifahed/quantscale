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

import org.qslib.quantscale.pattern.Observable
import org.saddle.Vec
import org.saddle.Mat
import org.joda.time.LocalDate
import scala.util.Try
import org.qslib.quantscale.pattern.Updatable

/**
 * Multi-dimensional stochastic process class.
 * This class describes a stochastic process governed by:
 *
 * dx(t)=mu(t,x(t))dt + sigma(t,x(t))dW(t)
 */
// FIXME Find a way to include math formulae in Scaladoc
trait StochasticProcess extends Updatable with Observable {

  /** @return the number of dimensions of the stochastic process */
  def size(): Int

  /** @return the number of independent factors of the process */
  def factors(): Int = size()

  /** @return the initial values of the state variables */
  def initialValues(): Option[Vec[Real]]

  /** @return the drift part of the equation, i.e. mu(t,x(t)) */
  def drift(t: Time, x: Vec[Real]): Try[Vec[Real]]

  /** @return the diffusion part of the equation, i.e. sigma(t,x(t)) */
  def diffusion(t: Time, x: Vec[Real]): Try[Mat[Real]]

  /**
   * @return the expectation E(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  def expectation(t0: Time, x0: Vec[Real], dt: Time): Try[Vec[Real]]

  /**
   * @return the standard deviation S(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  def stdDeviation(t0: Time, x0: Vec[Real], dt: Time): Try[Mat[Real]]

  /**
   * @return the covariance V(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  def covariance(t0: Time, x0: Vec[Real], dt: Time): Try[Mat[Real]]

  /**
   * By default, it returns:
   * E(x0, t0, delta t) + S(x0, t0, delta t).delta w
   * where E is the expectation and S the standard deviation.
   *
   * @return the asset value after a time interval delta t according to the
   * given discretization.
   */
  def evolve(t0: Time, x0: Vec[Real], dt: Time, dw: Vec[Real]): Try[Vec[Real]]

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

  // FIXME is this actually used? Is it worth implementing Observer and Observable?
  final override def update() {
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

  override final def expectation(t0: Time, x0: Vec[Real], dt: Time): Try[Vec[Real]] =
    for (d <- discretization.drift(this, t0, x0, dt)) yield apply(x0, d)

  override final def stdDeviation(t0: Time, x0: Vec[Real], dt: Time): Try[Mat[Real]] =
    discretization.diffusion(this, t0, x0, dt)

  override final def covariance(t0: Time, x0: Vec[Real], dt: Time): Try[Mat[Real]] =
    discretization.covariance(this, t0, x0, dt)

  override final def evolve(t0: Time, x0: Vec[Real], dt: Time, dw: Vec[Real]): Try[Vec[Real]] =
    for {
      e <- expectation(t0, x0, dt)
      s <- stdDeviation(t0, x0, dt)
    } yield apply(e, (s dot dw) col 0)

  override final def apply(x0: Vec[Real], dx: Vec[Real]): Vec[Real] = x0 + dx

  def discretization: DiscretizationND
}

/** Multi dimensional discretization of a stochastic process over a given time interval. */
trait DiscretizationND {
  def drift(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Try[Vec[Real]]
  def diffusion(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Try[Mat[Real]]
  def covariance(process: StochasticProcess, t0: Time, x0: Vec[Real], dt: Time): Try[Mat[Real]]
}

/**
 * ==1-Dimensional Stochastic Process==
 *
 * This trait describes a stochastic process governed by:
 * dx(t) = mu(t,x(t)) dt + sigma(t,x(t)) dW(t)
 */
trait StochasticProcess1D extends StochasticProcess {

  /** @return the initial value of the state variable */
  def x0(): Option[Real]

  /** @return the drift part of the equation, i.e. mu(t,x(t)) */
  def drift(t: Time, x: Real): Try[Real]

  /** @return the diffusion part of the equation, i.e. sigma(t,x(t)) */
  def diffusion(t: Time, x: Real): Try[Real]

  def discretization: Discretization1D

  /**
   * @return the expectation E(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  final def expectation(t0: Time, x0: Real, dt: Time): Try[Real] =
    for (e <- discretization.drift(this, t0, x0, dt)) yield apply(x0, e)

  /**
   * @return the standard deviation S(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  final def stdDeviation(t0: Time, x0: Real, dt: Time): Try[Real] =
    discretization.diffusion(this, t0, x0, dt)

  /**
   * @return the variance V(x(t0+delta t) | x(t0)=x0) of the process
   * after a time interval delta t according to the given discretization.
   * @note This method can be overridden in derived classes which want to
   * hard-code a particular discretization.
   */
  final def variance(t0: Time, x0: Real, dt: Time): Try[Real] =
    discretization.variance(this, t0, x0, dt)

  /**
   * By default, it returns:
   * E(x0, t0, delta t) + S(x0, t0, delta t).delta w
   * where E is the expectation and S the standard deviation.
   *
   * @return the asset value after a time interval delta t according to the
   * given discretization.
   */
  def evolve(t0: Time, x0: Real, dt: Time, dw: Real): Try[Real] =
    for {
      e <- expectation(t0, x0, dt)
      s <- stdDeviation(t0, x0, dt)
    } yield apply(e, s * dw)

  /**
   * Applies a change to the asset value. By default, it returns:
   * x + delta x
   */
  def apply(x0: Real, dx: Real): Real = x0 + dx

  override final val size = 1

  override final def initialValues() = x0.map(Vec(_))

  override final def drift(t: Time, x: Vec[Real]) =
    for (d <- drift(t, x raw 0)) yield Vec(d)

  override final def diffusion(t: Time, x: Vec[Real]) =
    for (d <- diffusion(t, x raw 0)) yield Mat(Vec(d))

  override final def expectation(t0: Time, x0: Vec[Real], dt: Time) =
    for (e <- expectation(t0, x0 raw 0, dt)) yield Vec(e)

  override final def stdDeviation(t0: Time, x0: Vec[Real], dt: Time) =
    for (s <- stdDeviation(t0, x0 raw 0, dt)) yield Mat(Vec(s))

  override final def covariance(t0: Time, x0: Vec[Real], dt: Time) =
    for (v <- variance(t0, x0 raw 0, dt)) yield Mat(Vec(v))

  override final def evolve(t0: Time, x0: Vec[Real], dt: Time, dw: Vec[Real]) =
    for (e <- evolve(t0, x0 raw 0, dt, dw raw 0)) yield Vec(e)

  override final def apply(x0: Vec[Real], dx: Vec[Real]) = Vec(apply(x0 raw 0, dx raw 0))
}

/** Discretization of a 1-D stochastic process. */
trait Discretization1D {
  def drift(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Try[Real]
  def diffusion(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Try[Real]
  def variance(process: StochasticProcess1D, t0: Time, x0: Real, dt: Time): Try[Real]
}
