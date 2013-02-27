package org.qslib.quantscale.currency

import org.qslib.quantscale.math._
import org.qslib.quantscale.Currency

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
 Copyright (C) 2004, 2005, 2008 StatPro Italia srl

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
 * ==American Currencies==
 *
 * Object containing all American currencies.
 * Data from http://fx.sauder.ubc.ca/currency_table.html
 * and http://www.thefinancials.com/vortex/CurrencyFormats.html
 *
 * @author Choucri FAHED
 * @since 1.0
 */
object America {

  /**
   * ==Argentinian Peso==
   * The ISO three-letter code is ARS; the numeric code is 32.
   * It is divided in 100 centavos.
   */
  val ARS = Currency("Argentinian peso", "ARS", 32, "", "", 100, Rounding(), "%2% %1$.2f")

  /**
   * ==Brazilian Real==
   * The ISO three-letter code is BRL; the numeric code is 986.
   * It is divided in 100 centavos.
   */
  val BRL = Currency("Brazilian real", "BRL", 986, "R$", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==Canadian Dollar==
   * The ISO three-letter code is CAD; the numeric code is 124.
   * It is divided into 100 cents.
   */
  val CAD = Currency("Canadian dollar", "CAD", 124, "Can$", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==Mexican Peso==
   * The ISO three-letter code is MXN; the numeric code is 484.
   * It is divided in 100 centavos.
   */
  val MXN = Currency("Mexican peso", "MXN", 484, "Mex$", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==U.S. Dollar==
   * The ISO three-letter code is USD; the numeric code is 840.
   * It is divided in 100 cents.
   */
  val USD = Currency("U.S. dollar", "USD", 840, "$", "\u00a2", 100, Rounding(), "%3% %1$.2f")

}