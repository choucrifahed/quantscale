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
 Copyright (C) 2003, 2004, 2008 StatPro Italia srl

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
import scala.annotation.tailrec

case class LinearInterpolation(xValues: Vec[Real], yValues: Vec[Real]) extends Interpolation {

  @tailrec
  private[this] def loop(sAcc: Vec[Real], primitiveConstAcc: Vec[Real], i: Int): (Vec[Real], Vec[Real]) = {
    if (i == xValues.length) (sAcc, primitiveConstAcc)
    else {
      val dx = x(i) - x(i - 1)
      val s = (y(i) - y(i - 1)) / dx
      val primitiveConst = primitiveConstAcc.raw(i - 1) + dx * (y(i - 1) + 0.5 * dx * s)
      loop(sAcc concat Vec(s), primitiveConstAcc concat Vec(primitiveConst), i + 1)
    }
  }

  private[this] val (s, primitiveConst) = loop(Vec(), Vec(0.0), 1)

  protected override def value(in: Real): Real = {
    val i = locate(in)
    yValues.raw(i) + (in - x(i)) * s.raw(i)
  }

  protected override def primitiveImpl(in: Real): Real = {
    val i = locate(in)
    val dx = in - x(i)
    primitiveConst.raw(i) + dx * (y(i) + 0.5 * dx * s.raw(i))
  }

  protected override def derivativeImpl(x: Real): Real = s raw locate(x)

  protected override def secondDerivativeImpl(x: Real): Real = 0.0
}

case object LinearInterpolator extends Interpolator {
  override def apply(xValues: Vec[Real], yValues: Vec[Real]) = LinearInterpolation(xValues, yValues)
}
