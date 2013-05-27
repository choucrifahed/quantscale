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
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

/** Mixed (explicit/implicit) scheme for finite difference methods. */
case class MixedScheme(
  L: TridiagonalOperator,
  bcs: Seq[BoundaryCondition],
  theta: Real,
  dt: Time = 0.0) extends Evolver {

  final val I = Identity(L.size)
  final val explicitPart = if (theta != 1.0) Some(I - L * ((1.0 - theta) * dt)) else None
  final val implicitPart = if (theta != 0.0) Some(I + L * (theta * dt)) else None

  // Evolver Interface

  def withStep(newDt: Time): MixedScheme = this.copy(dt = newDt)

  def step(t: Time): MixedScheme = if (L.isTimeDependent) {
    // There is an explicit part
    val lEx = if (theta != 1.0) L.withTime(t) else L

    // There is an implicit part
    val lIm = if (theta != 0.0) lEx.withTime(t - dt) else lEx

    MixedScheme(lIm, bcs, theta, dt)
  } else this

  def apply(u: Vec[Real]): Vec[Real] = {
    // There is an explicit part
    val vEx = if (theta != 1.0) {
      val explicitPart = bcs.foldLeft(this.explicitPart.get)((ex, bc) => bc.applyBeforeApplying(ex))
      bcs.foldLeft(explicitPart(u))((a, bc) => bc.applyAfterApplying(a))
    } else u

    // There is an implicit part
    if (theta != 0.0) {
      val (implicitPart, vIm) = bcs.foldLeft((this.implicitPart.get, vEx))(
        (result, bc) => bc.applyBeforeSolving(result._1, result._2))
      bcs.foldLeft(implicitPart.solveFor(vIm))((a, bc) => bc.applyAfterSolving(a))
    } else vEx
  }
}

/** Crank-Nicolson scheme for finite difference methods. */
object CrankNicolson {
  def apply(L: TridiagonalOperator, bcs: Seq[BoundaryCondition]) = MixedScheme(L, bcs, 0.5)
}

/** Forward Euler scheme for finite difference methods. */
object ExplicitEuler {
  def apply(L: TridiagonalOperator, bcs: Seq[BoundaryCondition]) = MixedScheme(L, bcs, 0.0)
}

/** Backward Euler scheme for finite difference methods. */
object ImplicitEuler {
  def apply(L: TridiagonalOperator, bcs: Seq[BoundaryCondition]) = MixedScheme(L, bcs, 1.0)
}
