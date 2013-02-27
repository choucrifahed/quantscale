package org.qslib.quantscale.currency

import org.qslib.quantscale.math._

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
 Copyright (C) 2004, 2005 StatPro Italia srl

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
 * ==Asian Currencies==
 *
 * Object containing all Asian currencies.
 * Data from http://fx.sauder.ubc.ca/currency_table.html
 * and http://www.thefinancials.com/vortex/CurrencyFormats.html
 *
 * @author Choucri FAHED
 * @since 1.0
 */
object Asia {

  /**
   * ==Chinese Yuan==
   * The ISO three-letter code is CNY; the numeric code is 156.
   * It is divided in 100 fen.
   */
  val CNY = Currency("Chinese yuan", "CNY", 156, "Y", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==Honk Kong Dollar==
   * The ISO three-letter code is HKD; the numeric code is 344.
   * It is divided in 100 cents.
   */
  val HKD = Currency("Honk Kong dollar", "HKD", 344, "HK$", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==Indian Rupee==
   * The ISO three-letter code is INR; the numeric code is 356.
   * It is divided in 100 paise.
   */
  val INR = Currency("Indian rupee", "INR", 356, "Rs", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==Iraqi Dinar==
   * The ISO three-letter code is IQD; the numeric code is 368.
   * It is divided in 1000 fils.
   */
  val IQD = Currency("Iraqi dinar", "IQD", 368, "ID", "", 1000, Rounding(3), "%2% %1$.3f")

  /**
   * ==Japanese Yen==
   * The ISO three-letter code is JPY; the numeric code is 392.
   * It is divided into 100 sen.
   */
  val JPY = Currency("Japanese yen", "JPY", 392, "\u00a5", "", 100, Rounding(), "%3% %1$.0f")

  /**
   * ==South-Korean Won==
   * The ISO three-letter code is KRW; the numeric code is 410.
   * It is divided in 100 chon.
   */
  val KRW = Currency("South-Korean won", "KRW", 410, "W", "", 100, Rounding(), "%3% %1$.0f")

  /**
   * ==Saudi Riyal==
   * The ISO three-letter code is SAR; the numeric code is 682.
   * It is divided in 100 halalat.
   */
  val SAR = Currency("Saudi riyal", "SAR", 682, "SRls", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==Singapore Dollar==
   * The ISO three-letter code is SGD; the numeric code is 702.
   * It is divided in 100 cents.
   */
  val SGD = Currency("Singapore dollar", "SGD", 702, "S$", "", 100, Rounding(), "%3% %1$.2f")

}
