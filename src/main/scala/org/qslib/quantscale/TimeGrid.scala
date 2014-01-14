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
 Copyright (C) 2005, 2006 StatPro Italia srl

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

import org.qslib.quantscale.Implicits._
import org.saddle._

case class TimeGrid(times: Vec[Time], dt: Vec[Time], mandatoryTimes: Vec[Time]) {

  require(times.isAscending, "Times have to be in ascending order.")
  require(times.length == dt.length + 1,
    s"Times length (${times.length}) should be equal to dt length (${dt.length}) + 1.")

  /** @return The index i such that grid[i] = t. */
  def index(t: Time): Int = {
    val i = closestIndex(t)
    if (t ~= times.get(i)) i
    else if (t < first)
      throw new IllegalArgumentException(
        "using inadequate time grid: all nodes are later than the required time t = "
          + t + " (earliest node is t1 = " + first + ")")
    else if (t > last)
      throw new IllegalArgumentException(
        "using inadequate time grid: all nodes are earlier than the required time t = "
          + t + " (latest node is t1 = " + last + ")")
    else {
      val (j, k) = if (t > times.raw(i)) (i, i + 1) else (i - 1, i)
      throw new IllegalArgumentException(
        "using inadequate time grid: the nodes closest to the required time t = "
          + t + " are t1 = " + times.get(j) + " and t2 = " + times.get(k));
    }
  }

  /** @return The index i such that grid[i] is closest to t. */
  def closestIndex(t: Time): Int = {
    val result = times.lowerBound(t)

    if (result == 0) 0
    else if (result == size) size - 1
    else {
      val dt1 = times.raw(result) - t
      val dt2 = t - times.raw(result - 1)
      if (dt1 < dt2) result
      else result - 1
    }
  }

  /** @return The time on the grid closest to the given t. */
  def closestTime(t: Time): Time = times raw closestIndex(t)

  def dt(i: Int): Time = dt raw i

  def apply(i: Int): Time = times raw i

  def size: Int = times.length

  def isEmpty: Boolean = times.isEmpty

  /** Called begin() and front() in QuantLib. */
  def first: Time = times.first

  /** Called end() and back() in QuantLib. */
  def last: Time = times.last
}

object TimeGrid {

  private def adjacentDifference(v: Vec[Time]): Vec[Time] =
    v.rolling(2, w => w.raw(1) - w.raw(0))

  /** @return A regularly spaced time-grid. */
  def apply(end: Time, steps: Int): TimeGrid = {
    // We seem to assume that the grid begins at 0.
    // Let's enforce the assumption for the time being
    // (even though I'm not sure that I agree.)
    require(end > 0.0, "negative times not allowed")
    val dt = end / steps
    val times = Vec(0 to steps: _*) map (_ * dt)
    val mandatoryTimes = Vec(end)

    TimeGrid(times, vec.ones(steps).map(_ * dt), mandatoryTimes)
  }

  /**
   * Mandatory points are guaranteed to belong to the grid.
   * No additional points are added.
   * @return A time grid with mandatory time points.
   */
  def apply(mandatoryTimes: Vec[Time]): TimeGrid = {
    val sortedMD = mandatoryTimes.sorted

    // We seem to assume that the grid begins at 0.
    // Let's enforce the assumption for the time being
    // (even though I'm not sure that I agree.)
    require(sortedMD.first >= 0.0, "negative times not allowed")

    val uniqueMD = sortedMD.unique((t1, t2) => t1 ~= t2)
    val times = if (uniqueMD.first > 0.0) Vec(0.0) concat uniqueMD else uniqueMD
    val dt = adjacentDifference(times)

    TimeGrid(times, dt, uniqueMD)
  }

  /**
   * Mandatory points are guaranteed to belong to the grid.
   * Additional points are then added with regular spacing
   * between pairs of mandatory times in order to reach the
   * desired number of steps.
   * @return A time grid with mandatory time points.
   */
  def apply(mandatoryTimes: Vec[Time], steps: Int): TimeGrid = {
    val sortedMD = mandatoryTimes.sorted

    // We seem to assume that the grid begins at 0.
    // Let's enforce the assumption for the time being
    // (even though I'm not sure that I agree.)
    require(sortedMD.first >= 0.0, "negative times not allowed")

    val uniqueMD = sortedMD.unique((t1, t2) => t1 ~= t2)

    // The resulting timegrid have points at times listed in the input
    // list. Between these points, there are inner-points which are
    // regularly spaced.
    val dtMax: Time = if (steps == 0) {
      val diff = adjacentDifference(uniqueMD)
      val diff2 = if (diff.first.get == 0.0) diff.tail(diff.length - 1) else diff
      diff2.min.get
    } else uniqueMD.last / steps

    val times = uniqueMD.foldLeft(Vec(0.0))((acc, e) => {
      if (e != 0.0) {
        val last = acc.last.get
        // the nearest integer
        val nSteps = ((e - last) / dtMax + 0.5).toInt
        // at least one time step!
        val nSteps2 = if (nSteps != 0) nSteps else 1
        val dt = (e - last) / nSteps2

        val newTimes = Vec(1 to nSteps2: _*) map (last + _ * dt)
        acc concat newTimes
      } else acc
    })

    val dt = adjacentDifference(times)
    TimeGrid(times, dt, uniqueMD)
  }
}
