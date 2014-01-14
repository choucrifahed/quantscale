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
import org.qslib.quantscale.Implicits._

/** Encapsulates an array of grid points. It is used primarily in PDE calculations. */
case class TransformedGrid(
  grid: Vec[Real],
  transformedGrid: Vec[Real],
  dxm: Vec[Real],
  dxp: Vec[Real],
  dx: Vec[Real]) {

  def size = grid.length
  def grid(i: Int): Real = grid raw i
  def transformedGrid(i: Int): Real = transformedGrid raw i
  def dxm(i: Int): Real = dxm raw i
  def dxp(i: Int): Real = dxp raw i
  def dx(i: Int): Real = dx raw i
}

object TransformedGrid {
  def apply(grid: Vec[Real], f: Real => Real = x => x): TransformedGrid = {
    val n = grid.length
    val transformedGrid = grid map f
    val transformedGridI = transformedGrid.slice(1, n - 1)
    val transformedGridIM = transformedGrid.slice(0, n - 2)
    val transformedGridIP = transformedGrid.slice(2, n)
    val dxm = transformedGridI - transformedGridIM
    val dxp = transformedGridIP - transformedGridI
    val dx = dxm + dxp

    TransformedGrid(grid, transformedGrid, dxm, dxp, dx)
  }
}

object LogGrid {
  def apply(grid: Vec[Real]): TransformedGrid = TransformedGrid(grid, scala.math.log)
}
