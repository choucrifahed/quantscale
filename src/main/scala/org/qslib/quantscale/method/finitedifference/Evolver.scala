package org.qslib.quantscale.method.finitedifference

import org.qslib.quantscale._
import org.saddle.Vec

trait Evolver {
  def withStep(newDt: Time): Evolver
  def step(t: Time): Evolver
  def apply(u: Vec[Real]): Vec[Real]
}
