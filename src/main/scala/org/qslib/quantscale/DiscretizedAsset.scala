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
 Copyright (C) 2004, 2005, 2006 StatPro Italia srl

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
import org.saddle.vec
import org.qslib.quantscale.Implicits._

/**
 * Discretized asset trait used by numerical methods.
 */
trait DiscretizedAsset {

  def time: Time
  def method: Lattice
  def values: Vec[Real]
  protected def withValues(newVal: Vec[Real]): DiscretizedAsset

  /**
   * High-level interface:
   *
   * Users of discretized assets should use these methods in
   * order to initialize, evolve and take the present value of
   * the assets.  They call the corresponding methods in the
   * Lattice interface, to which we refer for
   * documentation.
   */

  @inline final def initialize(time: Time): DiscretizedAsset =
    method.initialize(this, time)

  @inline final def rollback(to: Time): DiscretizedAsset =
    method.rollback(this, to)

  @inline final def partialRollback(to: Time): DiscretizedAsset =
    method.partialRollback(this, to)

  @inline final def presentValue(): Real = method.presentValue(this)

  /**
   * Low-level interface:
   *
   * These methods (that developers should override when
   * deriving from DiscretizedAsset) are to be used by
   * numerical methods and not directly by users, with the
   * exception of adjustValues(), preAdjustValues() and
   * postAdjustValues() that can be used together with
   * partialRollback().
   */

  /**
   * This method should initialize the asset values to an Array
   * of the given size and with values depending on the
   * particular asset.
   */
  def reset(size: Int): DiscretizedAsset

  /**
   * This method will be invoked after rollback and before any
   * other asset (i.e., an option on this one) has any chance to
   * look at the values. For instance, payments happening at times
   * already spanned by the rollback will be added here.
   *
   * This method is not virtual; derived classes must override
   * the protected preAdjustValuesImpl() method instead.
   */
  final def preAdjustValues(): DiscretizedAsset =
    if (time ~= latestPreAdjustment) this
    else preAdjustValuesImpl().withLatestPreAdjustment(time)

  /**
   * This method will be invoked after rollback and after any
   * other asset had their chance to look at the values. For
   * instance, payments happening at the present time (and therefore
   * not included in an option to be exercised at this time) will be
   * added here.
   *
   * This method is not virtual; derived classes must override
   * the protected postAdjustValuesImpl() method instead.
   */
  final def postAdjustValues(): DiscretizedAsset =
    if (time ~= latestPostAdjustment) this
    else postAdjustValuesImpl().withLatestPostAdjustment(time)

  /*! This method performs both pre- and post-adjustment */
  final def adjustValues(): DiscretizedAsset =
    preAdjustValues().postAdjustValues()

  /**
   * This method returns the times at which the numerical
   * method should stop while rolling back the asset. Typical
   * examples include payment times, exercise times and such.
   *
   * @note The returned values are not guaranteed to be sorted.
   */
  def mandatoryTimes(): Vec[Time]

  /** This method checks whether the asset was rolled at the given time. */
  protected final def isOnTime(t: Time): Boolean = {
    val grid = method.timeGrid
    grid(grid.index(t)) ~= time
  }

  /** This method performs the actual pre-adjustment. */
  protected def preAdjustValuesImpl(): DiscretizedAsset

  /** This method performs the actual post-adjustment. */
  protected def postAdjustValuesImpl(): DiscretizedAsset

  protected def latestPreAdjustment: Time
  protected def withLatestPreAdjustment(newVal: Time): DiscretizedAsset

  protected def latestPostAdjustment: Time
  protected def withLatestPostAdjustment(newVal: Time): DiscretizedAsset
}

//! Useful discretized discount bond asset
trait DiscretizedDiscountBond extends DiscretizedAsset {
  override final val mandatoryTimes = Vec[Time]()

  override final def reset(size: Int): DiscretizedDiscountBond = {
    withValues(vec.ones(size))
  }

  override protected def withValues(newVal: Vec[Real]): DiscretizedDiscountBond
}

/**
 * Discretized option on a given asset.
 *
 * WARNING: It is advised that derived classes take care of
 * creating and initializing themselves an instance of
 * the underlying.
 */
trait DiscretizedOption extends DiscretizedAsset {
  def underlying: DiscretizedAsset
  protected def withUnderlying(newUL: DiscretizedAsset): DiscretizedOption

  def exercise: Exercise

  // TODO move this method to Exercise?
  def exerciseTimes: Vec[Time]

  override final def reset(size: Int): DiscretizedAsset = {
    require(method == underlying.method,
      "option and underlying were initialized on different methods")
    withValues(vec.zeros(size)).adjustValues()
  }

  override final val mandatoryTimes: Vec[Time] = {
    val ulTimes = underlying.mandatoryTimes()
    // discard negative times...
    val posExerciseTimes = exerciseTimes.filter(_ >= 0.0)
    // and add the positive ones
    ulTimes concat posExerciseTimes
  }

  protected final def applyExerciseCondition(ul: DiscretizedAsset = underlying): DiscretizedOption = {
    val newValues = underlying.values.zipMap(values)((ulVal, myVal) => ulVal max myVal)
    withValues(newValues)
  }

  /**
   * In the real world, with time flowing forward, first
   * any payment is settled and only after options can be
   * exercised. Here, with time flowing backward, options
   * must be exercised before performing the adjustment.
   */
  override protected final def postAdjustValuesImpl(): DiscretizedOption = {
    val preUL = underlying.partialRollback(time).preAdjustValues()

    val newDO = exercise match {
      case _: AmericanExercise => {
        if (time >= exerciseTimes.first && time <= exerciseTimes.last)
          this applyExerciseCondition preUL
        else this
      }
      case _ => exerciseTimes.foldLeft(this)((doVal, t) =>
        if (t >= 0.0 && isOnTime(t)) doVal applyExerciseCondition preUL else doVal)
    }
    val postUL = preUL.postAdjustValues()

    newDO withUnderlying postUL
  }

  override protected def withValues(newVal: Vec[Real]): DiscretizedOption
}

