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

import org.saddle.Vec
import org.qslib.quantscale._
import org.qslib.quantscale.Implicits._
import scala.collection.SortedSet
import scala.annotation.tailrec
import scala.math._

/** Generic finite difference model. */
case class FiniteDifferenceModel(evolver: Evolver, stoppingTimeSet: SortedSet[Time]) {

  // Store times in a descending order sequence
  val stoppingTimes = stoppingTimeSet.toSeq.reverse

  /**
   * Solves the problem between the given times, applying a condition at every step.
   * WARNING: Being this a rollback, <tt>from</tt> must be a later time than <tt>to</tt>.
   */
  def rollback(u: Vec[Real], from: Time, to: Time, steps: Int,
    condition: Option[StepCondition] = None): Vec[Real] = {

    require(from >= to, s"Trying to roll back from $from to $to")
    val dt = (from - to) / steps
    val t = from
    val evolver0 = evolver withStep dt

    val u1 = if (!stoppingTimes.isEmpty && stoppingTimes.last == from && condition.isDefined)
      condition.get(u, from) else u

    // FIXME How ugly!
    @tailrec
    def loop1(i: Int, t: Time, u: Vec[Real], e: Evolver): Vec[Real] = if (i == steps) u
    else {
      val now = t
      val next = if (abs(to - (t - dt)) < sqrt(epsilon)) to else t - dt

      val hitTimes = stoppingTimes.filter(stop => next <= stop && stop < now)
      val (newE, newU) = if (!hitTimes.isEmpty) {
        val (u1, e1, now1) = hitTimes.foldLeft((u, e, now))((tup, stop) => {
          val a = tup._1
          val e = tup._2
          val now = tup._3

          val newE = e.withStep(now - stop).step(now)
          val newA = newE(a)
          (condition.map(c => c(newA, stop)).getOrElse(newA), newE, stop)
        })

        // if we did hit, we might have to make a small step to complete the big one
        val (e2, u2) = if (now1 > next) {
          val ev = e1.withStep(now1 - next).step(now1)
          val u = ev(u1)
          (ev, condition.map(c => c(u1, next)).getOrElse(u1))
        } else (e1, u1)
        // ...and in any case, we have to reset the evolver to the default step.
        val e3 = e2 withStep dt
        (e3, u2)
      } else {
        // if we didn't, the evolver is already set to the default step, which is ok for us.
        val ev = e.step(now)
        val u1 = ev(u)

        (ev, condition.map(c => c(u1, next)).getOrElse(u1))
      }

      loop1(i + 1, t - dt, newU, newE)
    }

    loop1(0, t, u1, evolver0)
  }
}
