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
 Copyright (C) 2004, 2005, 2006, 2007 StatPro Italia srl

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

import org.qslib.quantscale.math.Rounding

/**
 * Currency specification.
 *
 * @param name currency name, e.g, "U.S. Dollar"
 * @param code ISO 4217 three-letter code, e.g, "USD"
 * @param numericCode ISO 4217 numeric code, e.g, "840"
 * @param symbol symbol, e.g, "$"
 * @param fractionSymbol fraction symbol, e.g, "\u00a2"
 * @param fractionsPerUnit number of fractionary parts in a unit, e.g, 100
 * @param rounding rounding convention
 * @param format output format, it will be fed three positional parameters, namely, value, code, and symbol, in this order
 * @param triangulationCurrency currency used for triangulated exchange when required (optional)
 *
 * @author Choucri FAHED
 * @since 1.0
 */
final case class Currency(
  name: String,
  code: String,
  numericCode: Int,
  symbol: String,
  fractionSymbol: String,
  fractionsPerUnit: Int,
  rounding: Rounding,
  format: String,
  triangulationCurrency: Option[Currency] = None) {

  /** @return code */
  override def toString() = code

  /** Compares currency codes */
  override def equals(obj: Any) = obj.isInstanceOf[Currency] && obj.asInstanceOf[Currency].code == code

  /** @return code's hash code */
  override def hashCode = code.hashCode()

  /** Shortcut to declare money amounts such as EUR * 50.0 instead of Money(50.0, EUR) */
  def *(amount: Decimal)(implicit mcc: MoneyConversionConfig) = Money(amount, this)(mcc)
}
