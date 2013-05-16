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

package org.qslib.quantscale

import org.qslib.quantscale.currency.ExchangeRateManager
import org.saddle.Vec
import scala.reflect.ClassTag

/**
 * Implicit values and classes for syntactic sugar.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
object Implicits {

  // Implicit values
  // TODO Move this to a proper config file

  implicit val epsilon = 1.0e-5
  implicit val defaultPrecision = math.Precision(epsilon)
  implicit val defaultMoneyConversionConfig = MoneyConversionConfig(ExchangeRateManager, AutomatedConversion, EUR)

  /** Time interval used in finite differences. */
  implicit val dt = 0.0001

  // Implicit classes for syntactic sugar

  /** Shortcut to declare money amounts such as 50.0 * EUR instead of Money(50.0, EUR) */
  implicit class DecimalToMoney(val value: Decimal) extends AnyVal {
    def *(currency: Currency)(implicit mcc: MoneyConversionConfig) = Money(value, currency)(mcc)
    def *(money: Money)(implicit mcc: MoneyConversionConfig) = Money(value * money.value, money.currency)(mcc)
    def /(money: Money)(implicit mcc: MoneyConversionConfig) = Money(value / money.value, money.currency)(mcc)
  }

  /** Shortcut to tell if 2 decimals are close enough given a precision */
  implicit class AlmostEqualDecimal(val d: Decimal) extends AnyVal {
    def ~=(d2: Decimal)(implicit p: math.Precision): Boolean = {
      val diff = (d - d2).abs
      diff == 0.0 || diff <= d * p.p || diff <= d2 * p.p
    }
  }

  implicit class RichVec[T: Ordering: ClassTag](val vec: Vec[T]) {

    /** @return true if for every 2 consecutive elements x1 and x2, x1 < x2 */
    def isStrictlyAscending = isSorted((x1, x2) => implicitly[Ordering[T]].lt(x1, x2))

    /** @return true if for every 2 consecutive elements x1 and x2, x1 <= x2 */
    def isAscending = isSorted((x1, x2) => implicitly[Ordering[T]].lteq(x1, x2))

    /** @return true if for every 2 consecutive elements x1 and x2, x1 > x2 */
    def isStrictlyDescending = isSorted((x1, x2) => implicitly[Ordering[T]].gt(x1, x2))

    /** @return true if for every 2 consecutive elements x1 and x2, x1 >= x2 */
    def isDescending = isSorted((x1, x2) => implicitly[Ordering[T]].gteq(x1, x2))

    private def isSorted(order: (T, T) => Boolean): Boolean =
      vec.zipMap(vec.tail(vec.length - 1))(order).foldLeft(true)((b1, b2) => b1 && b2)

    /** @return a vector of unique elements keeping the original order of elements */
    def unique(same: (T, T) => Boolean): Vec[T] =
      vec.foldLeft(Vec(vec.first.get))((v, e) =>
        if (same(v.last.get, e)) v else v.concat(Vec(e)))

    def lowerBound(x: T, from: Int = 0, to: Int = vec.length): Int = {
      require(from >= 0 && from <= to && to <= vec.length)

      def loop(len: Int, from: Int): Int = {
        if (len == 0) from
        else {
          val half = len >> 1
          val middle = from + half
          if (implicitly[Ordering[T]].lt(x, vec raw middle)) {
            loop(len - (half + 1), middle + 1)
          } else {
            loop(half, from)
          }
        }
      }

      val len = to - from
      loop(len, from)
    }

    def upperBound(x: T, from: Int = 0, to: Int = vec.length): Int = {
      require(from >= 0 && from <= to && to <= vec.length)

      def loop(len: Int, from: Int): Int = {
        if (len == 0) from
        else {
          val half = len >> 1
          val middle = from + half
          if (implicitly[Ordering[T]].lt(x, vec raw middle)) {
            loop(half, from)
          } else {
            loop(len - (half + 1), middle + 1)
          }
        }
      }

      val len = to - from
      loop(len, from)
    }
  }
}
