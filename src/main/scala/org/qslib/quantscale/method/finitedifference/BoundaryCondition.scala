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

import org.saddle.Vec
import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._

/** Boundary condition trait for finite difference problems. */
trait BoundaryCondition {

  /**
   * Modifies an operator L before it is applied to an array u
   * so that v = Lu will satisfy the given condition.
   */
  def applyBeforeApplying(L: TridiagonalOperator): TridiagonalOperator

  /** Modifies an array u so that it satisfies the given condition. */
  def applyAfterApplying(u: Vec[Real]): Vec[Real]

  /**
   * Modifies an operator L before the linear system Lu' = u
   * is solved so that u' will satisfy the given condition.
   */
  def applyBeforeSolving(L: TridiagonalOperator, rhs: Vec[Real]): (TridiagonalOperator, Vec[Real])

  /** Modifies an array u so that it satisfies the given condition. */
  // TODO THIS METHOD DOES NOT DO ANYTHING
  def applyAfterSolving(u: Vec[Real]): Vec[Real]

  // THIS METHOD DOES NOT SEEM TO BE USED
  /*! This method sets the current time for time-dependent
            boundary conditions. */
  //virtual void setTime(Time t) = 0;
}

// Side Enum
sealed trait BCSide
case object Lower extends BCSide
case object Upper extends BCSide

/**
 * Neumann boundary condition (i.e., constant derivative).
 *
 * WARNING: The value passed must not be the value of the derivative.
 * Instead, it must be comprehensive of the grid step
 * between the first two points--i.e., it must be the
 * difference between f[0] and f[1].
 */
case class NeumannBC(value: Real, side: BCSide) extends BoundaryCondition {

  override def applyBeforeApplying(L: TridiagonalOperator): TridiagonalOperator =
    side match {
      case Lower => L.withFirstRow(-1.0, 1.0)
      case _ => L.withLastRow(-1.0, 1.0)
    }

  override def applyAfterApplying(u: Vec[Real]): Vec[Real] = side match {
    case Lower => Vec(u.raw(1) - value) concat u.tail()
    case _ => u.withoutLast() concat Vec(u.raw(u.length - 2) + value)
  }

  override def applyBeforeSolving(L: TridiagonalOperator, rhs: Vec[Real]) = side match {
    case Lower => (L.withFirstRow(-1.0, 1.0), Vec(value) concat rhs.tail())
    case _ => (L.withLastRow(-1.0, 1.0), rhs.withoutLast() concat Vec(value))
  }

  override def applyAfterSolving(u: Vec[Real]): Vec[Real] = u
}

/** Dirichlet boundary condition (i.e., constant value)*/
case class DirichletBC(value: Real, side: BCSide) extends BoundaryCondition {

  override def applyBeforeApplying(L: TridiagonalOperator): TridiagonalOperator =
    side match {
      case Lower => L.withFirstRow(1.0, 0.0)
      case _ => L.withLastRow(0.0, 1.0)
    }

  override def applyAfterApplying(u: Vec[Real]): Vec[Real] = side match {
    case Lower => Vec(value) concat u.tail()
    case _ => u.withoutLast() concat Vec(value)
  }

  override def applyBeforeSolving(L: TridiagonalOperator, rhs: Vec[Real]) = side match {
    case Lower => (L.withFirstRow(1.0, 0.0), Vec(value) concat rhs.tail())
    case _ => (L.withLastRow(0.0, 1.0), rhs.withoutLast() concat Vec(value))
  }

  override def applyAfterSolving(u: Vec[Real]): Vec[Real] = u
}
