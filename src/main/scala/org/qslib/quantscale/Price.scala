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
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2006 Joseph Wang

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

import org.joda.time.LocalDate
import org.saddle.Vec
import org.saddle.Series
import org.saddle.Mat

sealed trait PriceType
case object Bid extends PriceType
case object Ask extends PriceType
case object Last extends PriceType
case object Close extends PriceType

/** Mid price, calculated as the arithmetic average of bid and ask prices. */
case object Mid extends PriceType

/**
 * Mid equivalent price, calculated as
 * a) the arithmetic average of bid and ask prices when both are available
 * b) either the bid or the ask price if any of them is available
 * c) the last price or
 * d) the close price.
 */
case object EquivalentMid extends PriceType {
  def apply(bid: Option[Money], ask: Option[Money], last: Option[Money], close: Option[Money]): Option[Money] = {
    if (bid != None && ask != None) Some((bid.get + ask.get) / 2.0)
    else if (bid != None) bid
    else if (ask != None) ask
    else if (last != None) last
    else close
  }
}

/**
 * Safe Mid price, returns the mid price only if both
 * bid and ask are available.
 */
case object SafeMid extends PriceType {
  def apply(bid: Money, ask: Money): Money = (bid + ask) / 2.0
}

// Had to use an enumeration because of name collision with PriceType
// TODO Is this useful?
object IntervalPriceField extends Enumeration {
  type IntervalPriceField = Value
  val Open, Close, High, Low = Value
}
import IntervalPriceField._

case class IntervalPrice(open: Money, close: Money, high: Money, low: Money) {
  import IntervalPrice._
  def apply(field: IntervalPriceField) = field match {
    case Open => open
    case IntervalPriceField.Close => close
    case High => high
    case Low => low
  }
}

object IntervalPrice {
  def makeSeries(
    dates: Vec[LocalDate],
    open: Vec[Money],
    close: Vec[Money],
    high: Vec[Money],
    low: Vec[Money]): TimeSeries[IntervalPrice] = {

    require(open.length == close.length, "Open and close price vectors should must the same length!")
    require(open.length == high.length, "Open and high price vectors should must the same length!")
    require(open.length == low.length, "Open and low price vectors should must the same length!")

    val values = Mat(open, close, high, low).rows map {
      v => IntervalPrice(v.raw(0), v.raw(1), v.raw(2), v.raw(3))
    }

    Series(Vec(values: _*), dates)
  }

  def extractValues(intervalPriceTimeSeries: TimeSeries[IntervalPrice], field: IntervalPriceField): Vec[Money] =
    intervalPriceTimeSeries.values.map(intervalPrice => intervalPrice(field))

  def extractComponent(intervalPriceTimeSeries: TimeSeries[IntervalPrice], field: IntervalPriceField): TimeSeries[Money] =
    intervalPriceTimeSeries.mapValues(intervalPrice => intervalPrice(field))
}
