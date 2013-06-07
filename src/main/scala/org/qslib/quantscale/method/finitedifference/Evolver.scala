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
import org.saddle.Vec
import org.qslib.quantscale.math.TridiagonalOperator

trait Evolver {
  def withStep(newDt: Time): Evolver
  def step(t: Time): Evolver
  def apply(u: Vec[Real]): Vec[Real]
}

case class ParallelEvolver(evolvers: Seq[Evolver]) extends Evolver {
  override def withStep(newDt: Time) = ParallelEvolver(evolvers.map(_.withStep(newDt)))
  override def step(t: Time) = ParallelEvolver(evolvers.map(_.step(t)))

  // FIXME this is incorrect: have to apply on a subset of u
  override def apply(u: Vec[Real]): Vec[Real] = ???
}

trait EvolverFactory {
  def theta: Real
  def apply(L: TridiagonalOperator, bcs: Seq[BoundaryCondition]) = MixedScheme(L, bcs, theta)

  // FIXME this is incomplete have a take a subset of L
  def par(L: TridiagonalOperator, bcs: Seq[BoundaryCondition]) =
    ParallelEvolver((1 to L.size).map(i => apply(L, bcs)))
}

/** Crank-Nicolson scheme for finite difference methods. */
case object CrankNicolson extends EvolverFactory {
  override val theta = 0.5
}

/** Forward Euler scheme for finite difference methods. */
case object ExplicitEuler extends EvolverFactory {
  override val theta = 0.0
}

/** Backward Euler scheme for finite difference methods. */
case object ImplicitEuler extends EvolverFactory {
  override val theta = 1.0
}
