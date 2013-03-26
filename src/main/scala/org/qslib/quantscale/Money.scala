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

package org.qslib.quantscale

import scala.util.Try
import scala.util.Success
import scala.util.Failure

import org.qslib.quantscale._
import org.qslib.quantscale.currency._
import org.qslib.quantscale.math._

/**
 * Amount of cash.
 *
 * @param decimal value
 * @param currency
 *
 * @author Choucri FAHED
 * @since 1.0
 */
final case class Money(value: Decimal = 0.0, currency: Currency)(implicit mcc: MoneyConversionConfig)
  extends Ordered[Money] {

  // FIXME currency formats are odd!
  // override def toString = currency.format.format(rounded().value, currency.code, currency.symbol)
  override def toString = rounded().value + " " + (if (currency.symbol != "") currency.symbol else currency.code)

  def convertTo(targetCur: Currency): Try[Money] = {
    if (currency != targetCur) {
      for {
        rate <- ExchangeRateManager.lookup(currency, targetCur)
        result <- rate.exchange(this)(mcc)
      } yield result.rounded
    } else Success(this)
  }

  def convertToBase(): Try[Money] = convertTo(mcc.baseCurrency)

  // Arithmetics and comparisons
  @inline def unary_+ = this
  @inline def unary_- = Money(-value, currency)(mcc)

  /**
   * Adds two cash amounts and returns the result as Money.
   * @throws IllegalArgumentException if amounts have different currencies and no conversion is specified
   */
  @throws[IllegalArgumentException]("if amounts have different currencies and no conversion is specified")
  @inline def +(that: Money) = add(that).get

  /**
   * Subtracts two cash amounts and returns the result as Money.
   * @throws IllegalArgumentException if amounts have different currencies and no conversion is specified
   */
  @throws[IllegalArgumentException]("if amounts have different currencies and no conversion is specified")
  @inline def -(that: Money) = subtract(that).get

  /**
   * Multiplies two cash amounts and returns the result as Money.
   * @throws IllegalArgumentException if amounts have different currencies and no conversion is specified
   */
  @throws[IllegalArgumentException]("if amounts have different currencies and no conversion is specified")
  @inline def *(that: Money) = multiply(that).get

  /**
   * Divides two cash amounts and returns the result as Money.
   * @throws IllegalArgumentException if amounts have different currencies and no conversion is specified
   */
  @throws[IllegalArgumentException]("if amounts have different currencies and no conversion is specified")
  @inline def /(that: Money) = divide(that).get

  @inline def *(that: Decimal) = Money(value * that, currency)(mcc)
  @inline def /(that: Decimal) = Money(value / that, currency)(mcc)
  @inline def *(that: ExchangeRate) = that.exchange(this)(mcc)

  /** Adds two cash amounts and returns the result as Try[Money]. */
  @inline def add(that: Money) = process(that) { (a, b) => a + b }

  /** Subtracts two cash amounts and returns the result as Try[Money]. */
  @inline def subtract(that: Money) = process(that) { (a, b) => a - b }

  /** Multiplies two cash amounts and returns the result as Try[Money]. */
  @inline def multiply(that: Money) = process(that) { (a, b) => a * b }

  /** Divides two cash amounts and returns the result as Try[Money]. */
  @inline def divide(that: Money) = process(that) { (a, b) => a / b }

  private def process(that: Money)(op: (Decimal, Decimal) => Decimal): Try[Money] = {
    if (currency == that.currency) Success(Money(op(value, that.value), currency)(mcc))
    else mcc.conversionType match {
      case BaseCurrencyConversion => for {
        m1 <- this.convertToBase()
        m2 <- that.convertToBase()
      } yield Money(op(m1.value, m2.value), m1.currency)(mcc)
      case AutomatedConversion => for {
        m2 <- that.convertTo(currency)
      } yield Money(op(value, m2.value), currency)(mcc)
      case NoConversion => Failure(new IllegalArgumentException("Cannot convert amounts of different currencies with no conversion specified."))
    }
  }

  @inline def rounded(): Money = Money(currency rounding value, currency)(mcc)

  /**
   * Unlike == (which relies on equals()) this method tries to check if two cash amounts are equal even if they have different currencies.
   * Therefore, equals() method does not have the same behavior as in Quantlib because it has to comply with hashCode().
   * Thus the default implementations of equals() and hashCode() are not overriden.
   */
  @inline def ===(that: Money) = compare(that) == 0

  /**
   * Result of comparing `this` with Money `that`.
   * First, tries to convert to the same currency,
   * then compares value amounts.
   *
   * @return `x` where:
   *   - `x < 0` when `this.value < that.value`
   *   - `x == 0` when `this.value == that.value`
   *   - `x > 0` when  `this.value > that.value`
   * @throws IllegalArgumentException if amounts have different currencies and no conversion is specified
   */
  @throws[IllegalArgumentException]("if amounts have different currencies and no conversion is specified")
  @inline def compare(that: Money): Int = {
    val comparison = handleConversion(that)((a, b) => a compare b)

    // This is where an exception can be thrown
    comparison.get
  }

  /**
   * Determines if 2 amounts of cash are almost equal or not.
   * @throws IllegalArgumentException if amounts have different currencies and no conversion is specified
   */
  import Implicits._
  @throws[IllegalArgumentException]("if amounts have different currencies and no conversion is specified")
  @inline def ~=(that: Money)(implicit p: Precision) = {
    val comparison = handleConversion(that)((a, b) => a.~=(b)(p))

    // This is where an exception can be thrown
    comparison.get
  }

  private def handleConversion[R](that: Money)(op: (Decimal, Decimal) => R): Try[R] = {
    if (currency == that.currency) Success(op(value, that.value))
    else mcc.conversionType match {
      case BaseCurrencyConversion => for {
        m1 <- this.convertToBase()
        m2 <- that.convertToBase()
      } yield op(m1.value, m2.value)
      case AutomatedConversion => for {
        m2 <- that.convertTo(currency)
      } yield op(value, m2.value)
      case NoConversion => Failure(new IllegalArgumentException("Cannot convert amounts of different currencies with no conversion specified."))
    }
  }
}

/**
 * ==Conversion Settings==
 *
 * These parameters are used for combining money amounts in different currencies.
 */
sealed trait ConversionType

/** Do not perform conversions. */
case object NoConversion extends ConversionType

/** Convert both operands to the base currency before converting. */
case object BaseCurrencyConversion extends ConversionType

/** Return the result in the currency of the first operand. */
case object AutomatedConversion extends ConversionType

case class MoneyConversionConfig(conversionType: ConversionType, baseCurrency: Currency)
