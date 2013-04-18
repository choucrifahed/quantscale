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

package org.qslib.quantscale

import org.joda.time.DurationFieldType

package object time {

  type TimeUnit = org.joda.time.DurationFieldType

  // Prefix TU (like TimeUnit) is necessary 
  // to avoid name conflict with Joda Time classes Days, Weeks...
  val TUDays = DurationFieldType.days()
  val TUWeeks = DurationFieldType.weeks()
  val TUMonths = DurationFieldType.months()
  val TUYears = DurationFieldType.years()

  type Day = Int

  object Month extends Enumeration {
    type Month = Value

    // Fix id to start at 1 instead of 0
    val Jan = Value(1, "January")
    val Feb = Value("February")
    val Mar = Value("March")
    val Apr = Value("April")
    val May = Value("May")
    val Jun = Value("June")
    val Jul = Value("July")
    val Aug = Value("August")
    val Sep = Value("September")
    val Oct = Value("October")
    val Nov = Value("November")
    val Dec = Value("December")
  }

  type Year = Int
}
