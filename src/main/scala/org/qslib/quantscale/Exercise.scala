package org.qslib.quantscale

import org.scala_tools.time.Imports._

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
 Copyright (C) 2001, 2002, 2003 Sadruddin Rejeb
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2006 StatPro Italia srl

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
 * Option exercise classes.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
trait Exercise {
  def date(index: Int): LocalDate = allDates(index)
  def lastDate: LocalDate = allDates.last

  /** @return all exercise dates */
  def allDates: Seq[LocalDate]
}

/**
 * ==European Option==
 *
 * A European option can only be exercised at one (expiry) date.
 */
case class EuropeanExercise(date: LocalDate) extends Exercise {
  val allDates = Seq(date)
}

/**
 * Early-exercise base class.
 *
 * @param payoffAtExpiry The payoff can be at exercise (the default) or at expiry.
 */
abstract class EarlyExercise(val payoffAtExpiry: Boolean) extends Exercise

/**
 * ==American Exercise==
 *
 * An American option can be exercised at any time between two
 * predefined dates; the first date might be omitted, in which
 * case the option can be exercised at any time before the expiry.
 */
case class AmericanExercise(earliestDate: LocalDate = MinDate, latestDate: LocalDate, override val payoffAtExpiry: Boolean = false)
  extends EarlyExercise(payoffAtExpiry) {
  require(earliestDate <= latestDate, "earliestDate > latestDate exercise date")

  val allDates = IndexedSeq(earliestDate, latestDate)
}

/**
 * ==Bermudan Exercise==
 *
 * A Bermudan option can only be exercised at a set of fixed dates.
 */
// Cannot be defined as a case class because input dates have to be sorted
class BermudanExercise(inputDates: Seq[LocalDate], override val payoffAtExpiry: Boolean = false) extends EarlyExercise(payoffAtExpiry) {
  require(!inputDates.isEmpty, "no exercise date given")

  val allDates = inputDates.sortWith((date1, date2) => date1 <= date2)

  override def equals(other: Any) = (other != null) && other.isInstanceOf[BermudanExercise] &&
    payoffAtExpiry == other.asInstanceOf[BermudanExercise].payoffAtExpiry &&
    allDates == other.asInstanceOf[BermudanExercise].allDates

  override def hashCode = payoffAtExpiry.hashCode + allDates.hashCode
  override def toString = "BermudanExercise(" + allDates + ")"
}

// Methods used for instance creation and pattern matching
object BermudanExercise {
  def apply(inputDates: Seq[LocalDate], payoffAtExpiry: Boolean = false) = new BermudanExercise(inputDates, payoffAtExpiry)
  def unapply(exercise: BermudanExercise): Option[(Seq[LocalDate], Boolean)] = Some((exercise.allDates, exercise.payoffAtExpiry))
}
