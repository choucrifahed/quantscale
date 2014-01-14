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

import org.saddle.Vec
import Math._

object Grid {

  def centered(center: Real, dx: Real, steps: Int): Vec[Real] =
    Vec(0 to steps: _*) map (i => center + (i - steps / 2.0) * dx)

  def bounded(xMin: Real, xMax: Real, steps: Int): Vec[Real] = {
    val x = xMin
    val dx = (xMax - xMin) / steps
    Vec(0 to steps: _*) map (i => x + i * dx)
  }

  def boundedLog(xMin: Real, xMax: Real, steps: Int): Vec[Real] = {
    val gridLogSpacing = (log(xMax) - log(xMin)) / steps
    val edx = exp(gridLogSpacing)
    Vec(0 to steps: _*) map (i => xMin * pow(edx, i))
  }
}
