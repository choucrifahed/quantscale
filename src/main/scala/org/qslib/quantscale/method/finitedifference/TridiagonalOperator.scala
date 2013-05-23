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
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl
 Copyright (C) 2011 Ferdinando Ametrano

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

package org.qslib.quantscale.method.finitedifference

import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._
import org.saddle._
import scala.annotation.tailrec

/**
 * Implementation for tridiagonal operator.
 *
 * WARNING: to use real time-dependent algebra, you must overload
 * the corresponding operators in the inheriting time-dependent class.
 */
case class TridiagonalOperator(
  lowerDiagonal: Vec[Real],
  diagonal: Vec[Real],
  upperDiagonal: Vec[Real],
  time: Option[Time] = None) {

  val size: Int = diagonal.length

  require(lowerDiagonal.length == size - 1,
    s"Low diagonal vector of size ${lowerDiagonal.length} instead of ${size - 1}")
  require(upperDiagonal.length == size - 1,
    s"High diagonal vector of size ${upperDiagonal.length} instead of ${size - 1}")

  // Unary operators

  def unary_+(): TridiagonalOperator = this

  def unary_-(): TridiagonalOperator =
    TridiagonalOperator(-lowerDiagonal, -diagonal, -upperDiagonal, time)

  // Binary operators

  def +(o: TridiagonalOperator): TridiagonalOperator =
    TridiagonalOperator(lowerDiagonal + o.lowerDiagonal,
      diagonal + o.diagonal,
      upperDiagonal + o.upperDiagonal,
      time)

  def -(o: TridiagonalOperator): TridiagonalOperator =
    TridiagonalOperator(lowerDiagonal - o.lowerDiagonal,
      diagonal - o.diagonal,
      upperDiagonal - o.upperDiagonal,
      time)

  def *(o: Real): TridiagonalOperator =
    TridiagonalOperator(lowerDiagonal * o, diagonal * o, upperDiagonal * o, time)

  def /(o: Real): TridiagonalOperator =
    TridiagonalOperator(lowerDiagonal / o, diagonal / o, upperDiagonal / o, time)

  // Inspectors

  def isTimeDependent: Boolean = time.isDefined

  // Modifiers - this trait is meant to be immutable, therefore a copy is returned

  def withFirstRow(valB: Real, valC: Real): TridiagonalOperator = {
    val newD = diagonal.update(0, valB)
    val newUD = upperDiagonal.update(0, valC)

    TridiagonalOperator(lowerDiagonal, newD, newUD, time)
  }

  def withMidRow(i: Int, valA: Real, valB: Real, valC: Real): TridiagonalOperator = {
    require(i >= 1 && i <= size - 2, s"i ($i) is out of range size ($size)")

    val newLD = lowerDiagonal.update(i - 1, valA)
    val newD = diagonal.update(i, valB)
    val newUD = upperDiagonal.update(i, valC)

    TridiagonalOperator(newLD, newD, newUD, time)
  }

  def withMidRows(valA: Real, valB: Real, valC: Real): TridiagonalOperator = {
    val ul = vec.ones(size - 3)
    val d = vec.ones(size - 2)

    val newLD = ul.mapValues(_ * valA) concat Vec(lowerDiagonal.raw(size - 2))
    val newD = Vec(diagonal raw 0) concat d.mapValues(_ * valB) concat Vec(diagonal.raw(size - 1))
    val newUD = Vec(upperDiagonal raw 0) concat ul.mapValues(_ * valC)

    TridiagonalOperator(newLD, newD, newUD, time)
  }

  def withLastRow(valA: Real, valB: Real): TridiagonalOperator = {
    val newLD = lowerDiagonal.update(size - 2, valA)
    val newD = diagonal.update(size - 1, valB)

    TridiagonalOperator(newLD, newD, upperDiagonal, time)
  }

  def withTime(t: Time): TridiagonalOperator =
    TridiagonalOperator(lowerDiagonal, diagonal, upperDiagonal, Some(t))

  // Operator interface

  /** Apply operator to a given vector. */
  def apply(v: Vec[Real]): Vec[Real] = {
    require(v.length == size, s"Vector of the wrong size ${v.length} instead of $size.")

    val zero = Vec(0)
    val diagProd = diagonal * v
    val lowerProd = (lowerDiagonal * v.slice(0, size - 1)) concat zero
    val upperProd = zero concat (upperDiagonal * v.slice(1, size))

    lowerProd + diagProd + upperProd
  }

  /** Solve linear system for a given right-hand side. */
  def solveFor(rhs: Vec[Real]): Vec[Real] = {
    require(rhs.length == size, s"Right hand side vector of the wrong size ${rhs.length} instead of $size.")

    val bet = diagonal raw 0
    require(!(bet ~= 0.0), s"diagonal's first element ($bet) cannot be close to zero")

    val firstGuess = Vec((rhs raw 0) / bet)
    val temp = upperDiagonal / bet

    @tailrec
    def loop1(r: Vec[Real], j: Int): Vec[Real] =
      if (r.length == size) r
      else {
        val bet = diagonal.raw(j) - lowerDiagonal.raw(j - 1) * temp.raw(j - 1)
        if (bet ~= 0.0) throw new IllegalStateException("Cannot divide by zero")
        val lastVal = (rhs.raw(j) - lowerDiagonal.raw(j - 1) * r.last) / bet
        loop1(r concat Vec(lastVal), j + 1)
      }

    val result1 = loop1(firstGuess, 1)

    @tailrec
    def loop2(r: Vec[Real], j: Int): Vec[Real] =
      if (r.length == size) r
      else {
        val lastVal = result1.raw(j) - temp.raw(j) * result1.raw(j + 1)
        loop2(Vec(lastVal) concat r, j - 1)
      }

    loop2(Vec(result1.last), size - 2)
  }

  /** Solve linear system with the Successive Over-Relaxation (SOR) approach. */
  def SOR(rhs: Vec[Real], tolerance: Real): Vec[Real] = {
    require(rhs.length == size, s"Right hand side vector of the wrong size ${rhs.length} instead of $size.")

    val omega = 1.5

    @tailrec
    def sorIter(old: Vec[Real], sorIteration: Int, err: Real): Vec[Real] =
      if (err <= tolerance) old
      else {
        require(sorIteration < 100000,
          s"tolerance ($tolerance) not reached in $sorIteration  iterations. The error still is $err")

        val temp1 = omega * (rhs.raw(0) - upperDiagonal.raw(0) * old.raw(1) -
          diagonal.raw(0) * old.raw(0)) / diagonal.raw(0)
        val err1 = temp1 * temp1
        val firstEl = Vec(old.raw(0) + temp1)

        @tailrec
        def inner(r: Vec[Real], i: Int, e: Real): (Vec[Real], Real) =
          if (i == size - 1) (r, e)
          else {
            val temp2 = omega * (rhs.raw(i) - upperDiagonal.raw(i) * r.raw(i + 1) -
              diagonal.raw(i) * r.raw(i) - lowerDiagonal.raw(i - 1) * r.raw(i - 1)) / diagonal.raw(i)
            val err2 = e + temp1 * temp1
            inner(firstEl concat Vec(r.raw(i) + temp2), i + 1, err2)
          }
        val (result, err2) = inner(old, 1, err1)

        val temp3 = omega * (rhs.raw(size - 1) - diagonal.raw(size - 1) * old.raw(size - 1) -
          lowerDiagonal.raw(size - 2) * result.raw(size - 2)) / diagonal.raw(size - 1)
        val err3 = err2 + temp3 * temp3

        sorIter(result concat Vec(old.raw(size - 1) + temp3), sorIteration + 1, err3)
      }

    sorIter(rhs, 0, 2.0 * tolerance)
  }
}

object Zero {

  /** @return A square zero matrix. */
  def apply(size: Int): TridiagonalOperator = {
    val d = vec.zeros(size)
    val ul = vec.zeros(size - 1)
    TridiagonalOperator(ul, d, ul)
  }
}

object Identity {

  /** @return A square zero matrix. */
  def apply(size: Int): TridiagonalOperator = {
    val d = vec.ones(size)
    val ul = vec.zeros(size - 1)
    TridiagonalOperator(ul, d, ul)
  }
}
