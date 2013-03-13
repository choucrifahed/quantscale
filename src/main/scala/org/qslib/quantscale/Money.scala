package org.qslib.quantscale

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
 Copyright (C) 2004, 2005, 2006 StatPro Italia srl

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
 * Amount of cash.
 *
 * @param decimal value
 * @param currency
 *
 * @author Choucri FAHED
 * @since 1.0
 */
case class Money(value: Decimal = 0.0, currency: Currency) extends Ordered[Money] {

  override def toString = currency.format.format(rounded(), currency.code, currency.symbol)

  // Arithmetics and comparisons
  def unary_+ = this
  def unary_- = Money(-value, currency)

  def +(that: Money) = ???
  def -(that: Money) = ???
  def *(that: Money) = ???
  def *(that: Decimal) = ???
  def /(that: Money) = ???
  def /(that: Decimal) = ???

  //    /*! \relates Money */
  //    Money operator*(const Money&, Decimal);
  //    /*! \relates Money */
  //    Money operator*(Decimal, const Money&);
  //    /*! \relates Money */
  //    Money operator/(const Money&, Decimal);
  //    /*! \relates Money */
  //    Decimal operator/(const Money&, const Money&);
  //
  // FIXME override equals() and toString()
  //bool operator==(const Money&, const Money&);
  //    /*! \relates Money */
  //    bool operator!=(const Money&, const Money&);
  //
  //    /*! \relates Money */
  //    bool close(const Money&, const Money&, Size n = 42);
  //    /*! \relates Money */
  //    bool close_enough(const Money&, const Money&, Size n = 42);

  def rounded(): Money = Money(currency rounding value, currency)

  /**
   * Result of comparing `this` with operand `that`.
   *
   * Implement this method to determine how instances of Money will be sorted.
   *
   * Returns `x` where:
   *   - `x < 0` when `this < that`
   *   - `x == 0` when `this == that`
   *   - `x > 0` when  `this > that`
   */
  // FIXME take conversion into account!
  def compare(that: Money): Int = value compare that.value
}

/**
 * ==Conversion Settings==
 *
 * These parameters are used for combining money amounts in different currencies.
 */
object ConversionType extends Enumeration {
  type ConversionType = Value

  /** Do not perform conversions. */
  val NoConversion = Value

  /** Convert both operands to the base currency before converting. */
  val BaseCurrencyConversion = Value

  /** Return the result in the currency of the first operand. */
  val AutomatedConversion = Value
}
import ConversionType._

case class MoneyConversionConfig(conversionType: ConversionType, baseCurrency: Currency)

