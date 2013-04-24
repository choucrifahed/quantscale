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
 Copyright (C) 2007, 2011 Ferdinando Ametrano
 Copyright (C) 2007 Francois du Vignaud
 Copyright (C) 2004, 2005, 2007, 2009 StatPro Italia srl

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

import org.qslib.quantscale.pattern.ObservableValue
import org.joda.time.LocalDate
import org.qslib.quantscale.pattern.ObservableDefImpl

/** Global repository for run-time library settings. */
object Settings {

  /**
   * The date at which pricing is to be performed.
   * Client code can inspect the evaluation date, as in:
   * val today = Settings.evaluationDate()
   * where today's date is returned by default but can
   * set it to a new value, as in:
   * Settings.evaluationDate() = newDate
   * and can register with it, as in:
   * registerWith(Settings.evaluationDate())
   * to be notified when it is set to a new value.
   *
   * @note A notification is not sent when the evaluation
   * date changes for natural causes---i.e., a date
   * was not explicitly set (which results in today's
   * date being used for pricing) and the current date
   *  changes as the clock strikes midnight.
   */
  val evaluationDate = new Setting(LocalDate.now)

  /**
   * This flag specifies whether or not Events occurring on the reference
   * date should, by default, be taken into account as not happened yet.
   * It can be overridden locally when calling the Event.hasOccurred() method.
   */
  val includeReferenceDateEvents = new Setting(false)

  /**
   * If set, this flag specifies whether or not CashFlows
   * occurring on today's date should enter the NPV.  When the
   * NPV date (i.e., the date at which the cash flows are discounted)
   * equals today's date, this flag overrides the behavior chosen for
   * includeReferenceDate. It cannot be overridden locally when calling
   * the CashFlow.hasOccurred() method.
   */
  // FIXME Is this flag used?
  val includeTodaysCashFlows = new Setting[Option[Boolean]](None)

  // FIXME Is this flag used?
  val enforcesTodaysHistoricFixings = new Setting(false)
}

class Setting[T](override val initialValue: T) extends ObservableValue[T] with ObservableDefImpl
