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
 Copyright (C) 2009 Ferdinando Ametrano
 Copyright (C) 2005 Joseph Wang

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

import org.qslib.quantscale.pattern.Observable
import com.github.nscala_time.time.Imports._
import org.qslib.quantscale.pattern.ObservableDefImpl

/**
 * Base class for events associated with a given date.
 * This class acts as a base class for the actual event implementations.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
trait Event extends Observable with Ordered[Event] {

  /** @return the date at which the event occurs */
  def date(): LocalDate

  /**
   * @param includeRefDate If includeRefDate is true, then an event has not occurred if its
   * date is the same as the refDate, i.e. this method returns false if
   * the event date is the same as the refDate.
   * @return true if an event has already occurred before a date
   */
  final def hasOccurred(refDate: LocalDate = Settings.evaluationDate(),
    includeRefDate: Boolean = Settings.includeReferenceDateEvents()): Boolean =
    if (includeRefDate) date < refDate else date <= refDate

  @inline final def compare(that: Event): Int = date compare that.date
}

case class SimpleEvent(date: LocalDate) extends Event with ObservableDefImpl
