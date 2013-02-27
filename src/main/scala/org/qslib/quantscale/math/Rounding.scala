package org.qslib.quantscale.math

import scala.math.abs
import scala.math.pow

import org.qslib.quantscale.Decimal

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
 Copyright (C) 2004 Decillion Pty(Ltd)

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

/**
 * == Rounding Methods ==
 * The rounding methods follow the OMG specification available at ftp://ftp.omg.org/pub/docs/formal/00-06-29.pdf
 *
 * WARNING: The names of the Floor and Ceiling methods might be misleading. Check the provided reference.
 */
sealed trait RoundingType

/** Do not round: return the number unmodified. */
case object None extends RoundingType

/**
 * The first decimal place past the precision will be rounded up. This differs from the OMG rule which
 * rounds up only if the decimal to be rounded is greater than or equal to the rounding digit.
 */
case object Up extends RoundingType

/** All decimal places past the precision will be truncated. */
case object Down extends RoundingType

/**
 * The first decimal place past the precision will be rounded up if greater than or equal
 * to the rounding digit; this corresponds to the OMG round-up rule.  When the rounding
 * digit is 5, the result will be the one closest to the original number, hence the name.
 */
case object Closest extends RoundingType

/**
 * Positive numbers will be rounded up and negative numbers will be rounded down using
 * the OMG round up and round down rules.
 */
case object Floor extends RoundingType

/**
 * Positive numbers will be rounded down and negative numbers will be rounded up using
 * the OMG round up and round down rules.
 */
case object Ceiling extends RoundingType

/**
 * Basic rounding class.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
case class Rounding(precision: Int, roundingType: RoundingType = Closest, digit: Int = 5) {

  def apply(value: Decimal): Decimal = {
    if (roundingType == None) value
    else {
      val mult = pow(10.0, precision)
      val neg = value < 0.0
      val absByMult = abs(value) * mult
      val integral = absByMult.asInstanceOf[Int]
      val modVal = absByMult - integral
      val lvalue = roundingType match {
        case Up if (modVal != 0.0) => integral + 1.0
        case Closest if (modVal >= (digit / 10.0)) => integral + 1.0
        case Floor if (!neg && modVal >= (digit / 10.0)) => integral + 1.0
        case Ceiling if (neg && modVal >= (digit / 10.0)) => integral + 1.0
        case _ => integral
      }

      val result = lvalue / mult
      if (neg) -result else result
    }
  }
}
