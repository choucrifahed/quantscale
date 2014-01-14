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

import com.github.nscala_time.time.Imports._
import scala.collection.generic.ImmutableSortedMapFactory
import scala.collection.generic.CanBuildFrom
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import org.saddle.Series
import org.saddle.Vec
import scala.reflect.ClassTag

object TimeSeries {

  /** This method initializes the history with a set of date / value pairs. */
  def apply[T: ClassTag](pairs: (LocalDate, T)*): TimeSeries[T] = Series(pairs: _*)

  /**
   * This method initializes the history with a set of values passed as two sequences,
   * the first containing dates and the second containing corresponding values.
   */
  def apply[T: ClassTag](values: Vec[T], dates: Vec[LocalDate]): TimeSeries[T] = {
    require(dates.length == values.length, "The date and value vectors should have the same length!")
    Series(values, dates)
  }

  /**
   * This method initializes the history with a set of values. Such values are assigned
   * to a corresponding number of consecutive dates starting from <b><i>firstDate</i></b> included.
   */
  def apply[T: ClassTag](values: Vec[T], firstDate: LocalDate): TimeSeries[T] = {
    val dates = (0 to (values.length - 1)).map(days => firstDate plusDays days).toArray
    Series(values, dates)
  }
}
