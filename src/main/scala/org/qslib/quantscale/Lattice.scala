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
 Copyright (C) 2004, 2005 StatPro Italia srl

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

/**
 * Lattice (tree, finite-differences) base trait.
 *
 * Methods are to be used by discretized assets and
 * must be overridden by developers implementing numerical
 * methods. Users are advised to use the corresponding
 * methods of DiscretizedAsset instead.
 */
trait Lattice {

  def timeGrid: TimeGrid

  /** Initialize an asset at the given time. */
  def initialize(da: DiscretizedAsset, time: Time): DiscretizedAsset

  /**
   * Roll back an asset until the given time, performing any
   * needed adjustment.
   */
  def rollback(da: DiscretizedAsset, to: Time): DiscretizedAsset

  /**
   * Roll back an asset until the given time, but do not perform
   * the final adjustment.
   */
  def partialRollback(da: DiscretizedAsset, to: Time): DiscretizedAsset

  /** Computes the present value of an asset. */
  def presentValue(da: DiscretizedAsset): Real

  // FIXME this is a smell, but we need it. We'll rethink it later.
  def grid(t: Time): Vec[Real]
}
