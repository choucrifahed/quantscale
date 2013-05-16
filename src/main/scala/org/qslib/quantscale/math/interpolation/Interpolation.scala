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
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl

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

import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._
import org.saddle.Vec
import org.saddle.scalar.Scalar.scalarUnboxD

/**
 * Base class for 1-D interpolations.
 *
 * Classes derived from this trait will provide interpolated values
 * from two sequences of equal length, representing discretized
 * values of a variable and a function of the former, respectively.
 */
trait Interpolation {

  require(xValues.length == yValues.length, "x values and y values should have the same length")
  require(xValues.length >= 2, "not enough points to interpolate, at least 2 are required")
  require(xValues.isStrictlyAscending, "x values have to be sorted")

  /** @note x values must be in ascending order. */
  def xValues(): Vec[Real]

  def yValues(): Vec[Real]

  final def apply(x: Real, extrapolate: Boolean = false): Real = {
    checkRange(x, extrapolate)
    value(x)
  }

  final def primitive(x: Real, extrapolate: Boolean = false): Real = {
    checkRange(x, extrapolate)
    primitiveImpl(x)
  }

  final def derivative(x: Real, extrapolate: Boolean = false): Real = {
    checkRange(x, extrapolate)
    derivativeImpl(x)
  }

  final def secondDerivative(x: Real, extrapolate: Boolean = false): Real = {
    checkRange(x, extrapolate)
    secondDerivativeImpl(x)
  }

  final def xMin(): Real = xValues.first

  final def xMax(): Real = xValues.last

  final def isInRange(x: Real): Boolean = (xMin <= x && x <= xMax) || (x ~= xMin) || (x ~= xMax)

  protected def value(x: Real): Real
  protected def primitiveImpl(x: Real, extrapolate: Boolean = false): Real
  protected def derivativeImpl(x: Real, extrapolate: Boolean = false): Real
  protected def secondDerivativeImpl(x: Real, extrapolate: Boolean = false): Real

  protected final def locate(x: Real): Int =
    if (x < xMin) 0
    else if (x > xMax) xValues.length - 2
    else xValues.upperBound(x, 0, xValues.length - 1 /* FIXME test the -1 */) - 1

  protected final def checkRange(x: Real, extrapolate: Boolean) {
    require(extrapolate || isInRange(x),
      "interpolation range is [" + xMin() + ", " + xMax() + "]: extrapolation at " + x + " not allowed")
  }
}

/** Base Interpolation factory trait */
trait Interpolator {
  def interpolate(xValues: Vec[Real], yValues: Vec[Real]): Interpolation
}
