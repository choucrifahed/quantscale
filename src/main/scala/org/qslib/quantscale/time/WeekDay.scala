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
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl
 Copyright (C) 2004, 2005, 2006 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2006 Toyin Akin

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

package org.qslib.quantscale.time

import org.joda.time.DateTimeConstants
import org.joda.time.LocalDate

/**
 * ==WeekDay Enum==
 *
 * Unlike QuantLib Sunday = 1 rather than 7 for compatibility with Joda Time.
 * By the way, WEEKDAY Excel function behaves the same.
 *
 * @author Choucri FAHED
 */
object WeekDay extends Enumeration {

  val Monday = Value(1, "Monday", "Mon", "Mo")
  val Tuesday = Value(2, "Tuesday", "Tue", "Tu")
  val Wednesday = Value(3, "Wednesday", "Wed", "We")
  val Thursday = Value(4, "Thursday", "Thu", "Th")
  val Friday = Value(5, "Friday", "Fri", "Fr")
  val Saturday = Value(6, "Saturday", "Sat", "Sa")
  val Sunday = Value(7, "Sunday", "Sun", "Su")

  class WeekDayVal(i: Int, name: String, val short: String, val shortest: String) extends Val(i, name)
  protected final def Value(i: Int, name: String, short: String, shortest: String): WeekDayVal =
    new WeekDayVal(i, name, short, shortest)

  def apply(date: LocalDate): Value = this(date.getDayOfWeek())
}
