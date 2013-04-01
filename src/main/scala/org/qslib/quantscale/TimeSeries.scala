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
 Copyright (C) 2006 Joseph Wang
 Copyright (C) 2010 Liquidnet Holdings Inc.

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

import org.scala_tools.time.Imports._
import scala.collection.generic.ImmutableSortedMapFactory
import scala.collection.generic.CanBuildFrom
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

/**
 * ==Container for Historical Data==
 *
 * This class acts as a generic repository for a set of historical data.
 * Any single datum can be accessed through its date, while sets of
 * consecutive data can be accessed through iterators.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
class TimeSeries[T](val map: SortedMap[LocalDate, T]) {

  /** @return The (possibly None) datum corresponding to the given date */
  def apply(date: LocalDate): Option[T] = map get date

  /** @return The dates for which historical data exist */
  def dates(): Iterable[LocalDate] = map.keys

  /** @return The historical data */
  def values(): Iterable[T] = map.values

  /** @return The first date for which a historical datum exists */
  def firstDate(): LocalDate = map.firstKey

  /** @return The last date for which a historical datum exists */
  def lastDate(): LocalDate = map.lastKey

  /** @return The first value historically */
  def firstValue(): T = map(firstDate)

  /** @return The last value historically */
  def lastValue(): T = map(lastDate)

  /** @return The number of historical data */
  def size(): Int = map.size

  /** @return Whether the series contains any data */
  def isEmpty(): Boolean = map.isEmpty
}

object TimeSeries {

  /** This method initializes the history with a set of date / value pairs. */
  def apply[T](pairs: (LocalDate, T)*): TimeSeries[T] = new TimeSeries(TreeMap(pairs: _*))

  /**
   * This method initializes the history with a set of values passed as two sequences,
   * the first containing dates and the second containing corresponding values.
   */
  def apply[T](dates: Seq[LocalDate], values: Seq[T]): TimeSeries[T] = {
    require(dates.size == values.size, "The date and value sequence should have the same length!")

    val pairs = dates zip values
    apply(pairs: _*)
  }

  /**
   * This method initializes the history with a set of values. Such values are assigned
   * to a corresponding number of consecutive dates starting from <b><i>firstDate</i></b> included.
   */
  def apply[T](firstDate: LocalDate, values: Seq[T]): TimeSeries[T] = {
    val dates = (0 to (values.size - 1)).map(days => firstDate plusDays days)
    apply(dates, values)
  }
}
