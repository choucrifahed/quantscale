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
 Copyright (C) 2001, 2002, 2003 Nicolas Di Cesare
 Copyright (C) 2004, 2008, 2009, 2011 Ferdinando Ametrano
 Copyright (C) 2009 Sylvain Bertrand

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

package org.qslib.quantscale.math.interpolation

import org.saddle.Vec
import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._
import scala.annotation.tailrec
import org.qslib.quantscale.math.TridiagonalOperator

/**
 * Cubic interpolation between discrete points.
 *
 * Cubic interpolation is fully defined when the f_i function values
 * at points x_i are supplemented with f'_i function derivative values.
 *
 * Different types of first derivative approximations are implemented,
 * both local and non-local. Local schemes (Fourth-order, Parabolic,
 * Modified Parabolic, Fritsch-Butland, Akima, Kruger) use only f values
 * near x_i to calculate each f'_i. Non-local schemes (Spline with
 * different boundary conditions) use all f_i values and obtain
 * f'_i by solving a linear system of equations. Local schemes
 * produce C^1 interpolants, while the spline schemes generate C^2
 * interpolants.
 *
 * Hyman's monotonicity constraint filter is also implemented: It can be
 * applied to all schemes to ensure that in the regions of local
 * monotoniticity of the input (three successive increasing or decreasing
 * values) the interpolating cubic remains monotonic. If the interpolating
 * cubic is already monotonic, the Hyman filter leaves it unchanged
 * preserving all its original features.
 *
 * In the case of C^2 interpolants the Hyman filter ensures local
 * monotonicity at the expense of the second derivative of the interpolant
 * which will no longer be continuous in the points where the filter has
 * been applied.
 *
 * While some non-linear schemes (Modified Parabolic, Fritsch-Butland,
 * Kruger) are guaranteed to be locally monotonic in their original
 * approximation, all other schemes must be filtered according to the
 * Hyman criteria at the expense of their linearity.
 *
 * See R. L. Dougherty, A. Edelman, and J. M. Hyman,
 * "Nonnegativity-, Monotonicity-, or Convexity-Preserving CubicSpline and
 * Quintic Hermite Interpolation"
 * Mathematics Of Computation, v. 52, n. 186, April 1989, pp. 471-494.
 */
// FIXME Find a way to include math formulae in Scaladoc
case class CubicInterpolation(
  xValues: Vec[Real],
  yValues: Vec[Real],
  da: DerivativeApprox,
  leftCond: BoundaryCondition,
  leftCondVal: Real,
  rightCond: BoundaryCondition,
  rightCondVal: Real,
  monotonic: Boolean = false) extends Interpolation {

  private[this] def init(): (Vec[Real], Vec[Real], Vec[Real], Vec[Real]) = {
    val n = xValues.length

    @tailrec
    def loop(dxAcc: Vec[Real], sAcc: Vec[Real], i: Int): (Vec[Real], Vec[Real]) = {
      if (i == n - 1) (dxAcc, sAcc)
      else {
        val dx = x(i + 1) - x(i)
        val s = (y(i + 1) - y(i)) / dx
        loop(dxAcc concat Vec(dx), sAcc concat Vec(s), i + 1)
      }
    }

    // dx and s of size n-1
    val (dx, s) = loop(Vec(), Vec(), 0)

    // First derivative approximation, tmp of size n
    val tmp = da match {

      case Spline => {

        // Left boundary condition
        val (firstD, firstHD, firstTmp) = leftCond match {
          case SecondDerivative => (Vec(2.0), Vec(1.0), Vec(3.0 * s.raw(0) - leftCondVal * dx.raw(0) / 2.0))
          case _ => ???
        }

        val midLD = dx.tail()
        val midHD = dx.withoutLast()
        val midD = (midLD + midHD) * 2.0
        val midTmp = (midLD * s.withoutLast + midHD * s.tail()) * 3.0

        // Right boundary condition
        val (lastLD, lastD, lastTmp) = rightCond match {
          case SecondDerivative => (Vec(1.0), Vec(2.0), Vec(3.0 * s.raw(n - 2) - rightCondVal * dx.raw(n - 2) / 2.0))
          case _ => ???
        }

        // Build the operator
        val L = TridiagonalOperator(midLD concat lastLD, firstD concat midD concat lastLD, firstHD concat midHD)

        // Solve the system
        L.solveFor(firstTmp concat midTmp concat lastTmp)
      }
      case _ => ???
    }

    val tmpI = tmp.withoutLast()
    val tmpIPlus1 = tmp.tail()
    val a = tmpI
    val b = (s * 3.0 - tmpIPlus1 - tmpI * 2.0) / dx
    val c = (tmpIPlus1 + tmpI - s * 2.0) / (dx * dx)

    @tailrec
    def loopPrimitiveConst(i: Int, acc: Vec[Real]): Vec[Real] =
      if (i == n - 1) acc
      else {
        val primitiveConst = acc.last.get +
          dx.raw(i - 1) * (y(i - 1) +
            dx.raw(i - 1) * (a.raw(i - 1) / 2.0 +
              dx.raw(i - 1) * (b.raw(i - 1) / 3.0 +
                dx.raw(i - 1) * c.raw(i - 1) / 4.0)))
        loopPrimitiveConst(i + 1, acc concat Vec(primitiveConst))
      }

    val primitiveConst = loopPrimitiveConst(1, Vec(0.0))

    (a, b, c, primitiveConst)
  }

  private[this] val (a, b, c, primitiveConst): (Vec[Real], Vec[Real], Vec[Real], Vec[Real]) = init()

  protected override def value(in: Real): Real = {
    val j = locate(in)
    val dx = in - x(j)
    y(j) + dx * (a.raw(j) + dx * (b.raw(j) + dx * c.raw(j)))
  }

  protected override def primitiveImpl(in: Real): Real = {
    val j = locate(in)
    val dx = in - x(j)
    primitiveConst.raw(j) + dx * (y(j) + dx * (a.raw(j) / 2.0
      + dx * (b.raw(j) / 3.0 + dx * c.raw(j) / 4.0)))
  }

  protected override def derivativeImpl(in: Real): Real = {
    val j = locate(in)
    val dx = in - x(j)
    a.raw(j) + dx * (2.0 * b.raw(j) + 3.0 * dx * c.raw(j))
  }

  protected override def secondDerivativeImpl(in: Real): Real = {
    val j = locate(in)
    val dx = in - x(j)
    2.0 * b.raw(j) + 6.0 * dx * c.raw(j)
  }
}

case object CubicNaturalSpline extends Interpolator {
  override def apply(x: Vec[Real], y: Vec[Real]): CubicInterpolation =
    CubicInterpolation(x, y, Spline, SecondDerivative, 0.0, SecondDerivative, 0.0)
}

sealed trait DerivativeApprox

/**
 * Spline approximation (non-local, non-monotonic, linear[?]).
 * Different boundary conditions can be used on the left and right
 * boundaries: see BoundaryCondition.
 */
case object Spline extends DerivativeApprox

/** Overshooting minimization 1st derivative. */
//case object SplineOM1 extends DerivativeApprox

/** Overshooting minimization 2nd derivative. */
//case object SplineOM2 extends DerivativeApprox

/** Fourth-order approximation (local, non-monotonic, linear). */
//case object FourthOrder extends DerivativeApprox

/** Parabolic approximation (local, non-monotonic, linear). */
//case object Parabolic extends DerivativeApprox

/** Fritsch-Butland approximation (local, monotonic, non-linear). */
//case object FritschButland extends DerivativeApprox

/** Akima approximation (local, non-monotonic, non-linear). */
//case object Akima extends DerivativeApprox

/** Kruger approximation (local, monotonic, non-linear). */
//case object Kruger extends DerivativeApprox

// -----------------------------------------------------------------

sealed trait BoundaryCondition

/** Make second(-last) point an inactive knot. */
//case object NotAKnot extends BoundaryCondition

/** Match value of end-slope. */
//case object FirstDerivative extends BoundaryCondition

/** Match value of second derivative at end. */
case object SecondDerivative extends BoundaryCondition

/** Match first and second derivative at either end. */
//case object Periodic extends BoundaryCondition

/**
 * Match end-slope to the slope of the cubic that matches
 * the first four data at the respective end
 */
//case object Lagrange extends BoundaryCondition
