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
import org.qslib.quantscale.instrument.PlainVanillaPayoff

/**
 * Abstract base trait which allows step conditions to use both
 * payoff and array functions.
 */
trait CurveDependentStepCondition extends StepCondition {

  final def apply(v: Vec[Real], t: Time): Vec[Real] =
    v.zipMap(Vec(0 until v.length: _*)) {
      case (value, i) => applyToValue(value, wrapper(v, i))
    }

  protected def applyToValue(value: Real, curveVal: Real): Real

  protected def wrapper: CurveDependentStepCondition.Wrapper
}

object CurveDependentStepCondition {
  type Wrapper = (Vec[Real], Int) => Real

  case class VecWrapper(vec: Vec[Real]) extends Wrapper {

    /** @param in is completely ignored. */
    override def apply(in: Vec[Real], i: Int): Real = vec raw i
  }

  case class PayoffWrapper(payoff: Payoff) extends Wrapper {

    /** @param in is completely ignored. */
    override def apply(in: Vec[Real], i: Int): Real = payoff(in raw i)
  }

  object PayoffWrapper {
    def apply(optionType: OptionType, strike: Money): PayoffWrapper =
      PayoffWrapper(PlainVanillaPayoff(optionType, strike))
  }

}

trait CurveDependentStepConditionFactory[T <: CurveDependentStepCondition] {
  def apply(wrapper: CurveDependentStepCondition.Wrapper): T

  def apply(optionType: OptionType, strike: Money): T =
    apply(CurveDependentStepCondition.PayoffWrapper(optionType, strike))

  def apply(p: Payoff): T = apply(CurveDependentStepCondition.PayoffWrapper(p))

  def apply(intrinsicValues: Vec[Real]): T =
    apply(CurveDependentStepCondition.VecWrapper(intrinsicValues))
}
