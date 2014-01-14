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

package org.qslib.quantscale.math

import org.saddle.Vec
import org.qslib.quantscale._
import scala.math._
import org.qslib.quantscale.math.interpolation.CubicNaturalSpline

/** This class contains a sampled curve. */
case class SampledCurve(grid: Vec[Real], values: Vec[Real]) {

  @inline def gridValue(i: Int): Real = grid raw i
  @inline def value(i: Int): Real = values raw i
  @inline def size(): Int = grid.length
  @inline def isEmpty(): Boolean = grid.isEmpty

  def valueAtCenter(): Real = {
    require(!isEmpty(), "Empty sampled curve")
    val jmid = size() / 2

    if (size() % 2 == 1) value(jmid)
    else (value(jmid) + value(jmid - 1)) / 2.0
  }

  def firstDerivativeAtCenter(): Real = {
    require(size() >= 3, "The size of the curve must be at least 3")
    val jmid = size() / 2

    if (size() % 2 == 1) {
      (value(jmid + 1) - value(jmid - 1)) / (grid.raw(jmid + 1) - grid.raw(jmid - 1))
    } else (value(jmid) - value(jmid - 1)) / (grid.raw(jmid) - grid.raw(jmid - 1))
  }

  def secondDerivativeAtCenter(): Real = {
    require(size() >= 4, "The size of the curve must be at least 4")
    val jmid = size() / 2

    if (size() % 2 == 1) {
      val deltaPlus = (value(jmid + 1) - value(jmid)) /
        (grid.raw(jmid + 1) - grid.raw(jmid))
      val deltaMinus = (value(jmid) - value(jmid - 1)) /
        (grid.raw(jmid) - grid.raw(jmid - 1))
      val dS = (grid.raw(jmid + 1) - grid.raw(jmid - 1)) / 2.0

      (deltaPlus - deltaMinus) / dS
    } else {
      val deltaPlus = (value(jmid + 1) - value(jmid - 1)) /
        (grid.raw(jmid + 1) - grid.raw(jmid - 1))
      val deltaMinus = (value(jmid) - value(jmid - 2)) /
        (grid.raw(jmid) - grid.raw(jmid - 2))

      (deltaPlus - deltaMinus) / (grid.raw(jmid) - grid.raw(jmid - 1))
    }
  }

  def shiftGrid(s: Real): SampledCurve = mapGrid(_ + s)
  def scaleGrid(s: Real): SampledCurve = mapGrid(_ * s)

  /** This method is called transformGrid() in QuantLib. */
  def mapGrid(f: Real => Real): SampledCurve = copy(grid = grid map f)

  /** This method is called transform() in QuantLib. */
  def map(f: Real => Real): SampledCurve = copy(values = values map f)

  def withLogGrid(min: Real, max: Real): SampledCurve = {
    val newGrid = Grid.boundedLog(min, max, size() - 1)
    copy(grid = newGrid)
  }

  def regridLogGrid(min: Real, max: Real): SampledCurve = {
    val newGrid = Grid.boundedLog(min, max, size() - 1)
    regrid(newGrid, log)
  }

  def regrid(newGrid: Vec[Real]): SampledCurve = {
    val priceSpline = CubicNaturalSpline(grid, values)
    SampledCurve(newGrid, newGrid.map(priceSpline(_, true)))
  }

  def regrid(newGrid: Vec[Real], f: Real => Real): SampledCurve = {
    val transformedGrid = grid map f
    val priceSpline = CubicNaturalSpline(transformedGrid, values)
    val splineF = (x: Real) => priceSpline(x, true)
    val newValues = newGrid.map(f andThen splineF)

    SampledCurve(newGrid, newValues)
  }
}

object SampledCurve {
  def apply(grid: Vec[Real], f: Real => Real): SampledCurve =
    SampledCurve(grid, grid map f)
}
