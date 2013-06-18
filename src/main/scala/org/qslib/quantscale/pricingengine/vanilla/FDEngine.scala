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
 Copyright (C) 2002, 2003, 2004 Ferdinando Ametrano
 Copyright (C) 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
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

package org.qslib.quantscale.pricingengine.vanilla

import org.qslib.quantscale.process.GeneralizedBlackScholesProcess
import org.qslib.quantscale._
import org.joda.time.LocalDate
import org.qslib.quantscale.math._
import org.saddle.Vec
import org.qslib.quantscale.method.finitedifference.BoundaryCondition
import org.qslib.quantscale.instrument.OneAssetOption
import org.qslib.quantscale.instrument.StrikedTypePayoff
import scala.math._
import scala.util.Try

/**
 * Finite-differences pricing engine for BSM one asset options.
 * This is a base trait for any finite difference scheme.
 * Its main job is to handle grid layout.
 */
trait FDEngineModule {
  final val safetyZoneFactor = 1.1
  final val minGridPoints = 10
  final val minGridPointsPerYear = 2

  type FDEngine <: FDEngineLike

  /** @return exerciseDate, payoff, requiredGridValue from the one asset option */
  protected def setupArguments(option: OneAssetOption): (LocalDate, StrikedTypePayoff, Real) = {
    val exerciseDate = option.exercise.lastDate
    val payoff = option.payoff
    val requiredGridValue = payoff.strikeValue

    (exerciseDate, payoff, requiredGridValue)
  }

  trait FDEngineLike extends PricingEngine {
    type InstrumentType <: OneAssetOption

    protected def init(gridPoints: Int) = {
      val t = getResidualTime()
      val newGridPoints = safeGridPoints(gridPoints, t)

      for {
        limits <- gridLimits(t)
        (sMin, sMax) = limits
        intrinsicValues = initializeInitialCondition(sMin, sMax, newGridPoints)
      } yield intrinsicValues
    }

    // Methods
    protected def gridLimits(t: Time): Try[(Real, Real)] = for {
      center <- Try(process.underlying().get)
      limits <- gridLimits(center, t)
    } yield ensureStrikeInGrid(limits._1, limits._2, center)

    //setGridLimits(process.underlying().get, getResidualTime()).ensureStrikeInGrid()

    protected def gridLimits(center: Real, t: Time): Try[(Real, Real)] =
      Try {
        require(center > 0.0, "negative or null underlying given")
      } flatMap { _ =>
        for {
          variance <- process.blackVolatility().blackVariance(t, center)
          volSqrtTime = sqrt(variance)
          // the prefactor fine tunes performance at small volatilities
          prefactor = 1.0 + 0.02 / volSqrtTime
          minMaxFactor = exp(4.0 * prefactor * volSqrtTime)
          sMin = center / minMaxFactor // underlying grid min value
          sMax = center * minMaxFactor // underlying grid max value
        } yield (sMin, sMax)
      }

    /** Safety check to be sure we have enough grid points. */
    private[this] def safeGridPoints(gridPoints: Int, residualTime: Time): Int = {
      val lowerLimit = if (residualTime > 1.0)
        (minGridPoints + (residualTime - 1.0) * minGridPointsPerYear).toInt
      else minGridPoints

      gridPoints max lowerLimit
    }

    /**
     * Ensure strike is included in the grid.
     *
     * @return adjusted sMin and sMax
     */
    protected def ensureStrikeInGrid(sMin: Real, sMax: Real, center: Real): (Real, Real) =
      if (sMin > requiredGridValue / safetyZoneFactor) {
        val adjSMin = requiredGridValue / safetyZoneFactor
        // enforce central placement of the underlying
        (adjSMin, center / (adjSMin / center))
      } else if (sMax < requiredGridValue * safetyZoneFactor) {
        val adjSMax = requiredGridValue * safetyZoneFactor
        // enforce central placement of the underlying
        (center / (adjSMax / center), adjSMax)
      } else (sMin, sMax)

    protected def initializeInitialCondition(sMin: Real, sMax: Real, gridPoints: Int): SampledCurve = {
      val newGrid = Grid.boundedLog(sMin, sMax, gridPoints - 1)
      SampledCurve(newGrid, payoff)
    }

    protected def initializeOperator(): FDEngine
    protected def initializeBoundaryConditions(): FDEngine
    protected def getResidualTime(): Time = process.time(exerciseDate)

    // Data
    protected def process: GeneralizedBlackScholesProcess
    protected def timeSteps: Int = 100
    protected def isTimeDependent: Boolean = false

    protected def requiredGridValue: Real
    protected def exerciseDate: LocalDate
    protected def payoff: StrikedTypePayoff
    protected def finiteDifferenceOperator: TridiagonalOperator
    protected def boundaryConditions: Vec[BoundaryCondition]

    // temporaries
    private[this] def gridLogSpacing: Real = ???

  }
}
