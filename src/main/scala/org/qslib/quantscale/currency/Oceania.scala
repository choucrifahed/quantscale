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
 * ==Oceanian Currencies==
 *
 * Object containing all Oceanian currencies.
 * Data from http://fx.sauder.ubc.ca/currency_table.html
 * and http://www.thefinancials.com/vortex/CurrencyFormats.html
 *
 * @author Choucri FAHED
 * @since 1.0
 */
object Oceania {

  /**
   * ==Australian Dollar==
   * The ISO three-letter code is AUD; the numeric code is 36.
   * It is divided into 100 cents.
   */
  val AUD = Currency("Australian dollar", "AUD", 36, "A$", "", 100, Rounding(), "%3% %1$.2f")

  /**
   * ==New Zealand Dollar==
   * The ISO three-letter code is NZD; the numeric code is 554.
   * It is divided in 100 cents.
   */
  val NZD = Currency("New Zealand dollar", "NZD", 554, "NZ$", "", 100, Rounding(), "%3% %1$.2f")

}
