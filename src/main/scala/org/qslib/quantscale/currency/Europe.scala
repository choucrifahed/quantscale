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
 * ==European Currencies==
 *
 * Object containing all European currencies.
 * Data from http://fx.sauder.ubc.ca/currency_table.html
 * and http://www.thefinancials.com/vortex/CurrencyFormats.html
 *
 * @author Choucri FAHED
 * @since 1.0
 */
object Europe {

  /**
   * ==Swiss Franc==
   * The ISO three-letter code is CHF; the numeric code is 756.
   * It is divided into 100 cents.
   */
  val CHF = Currency("Swiss franc", "CHF", 756, "SwF", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==European Euro==
   * The ISO three-letter code is EUR; the numeric code is 978.
   * It is divided into 100 cents.
   */
  val EUR = Currency("European Euro", "EUR", 978, "\u20ac", "", 100, Rounding(), "%2% %1$.2f")

  /**
   * ==British Pound Sterling==
   * The ISO three-letter code is GBP; the numeric code is 826.
   * It is divided into 100 pence.
   */
  val GBP = Currency("British pound sterling", "GBP", 826, "\u00a3", "p", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==Italian lira==
   * The ISO three-letter code was ITL; the numeric code was 380.
   * It had no subdivisions. Obsoleted by the Euro since 1999.
   */
  val ITL = Currency("Italian lira", "ITL", 380, "L", "", 0, Rounding(), "%3% %1$.2f", Some(EUR))

  /**
   * ==Swedish krona==
   * The ISO three-letter code is SEK; the numeric code is 752.
   * It is divided in 100 oere.
   */
  val SEK = Currency("Swedish krona", "SEK", 752, "kr", "", 100, Rounding(), "%1$.2f %3%")

}
