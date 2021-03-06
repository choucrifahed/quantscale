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
 Copyright (C) 2011, 2012 Ferdinando Ametrano

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

package org.qslib.quantscale.pattern

import java.util.concurrent.CopyOnWriteArraySet
import scala.concurrent.stm._

/**
 * Object that notifies its changes to a set of observers.
 */
trait Observable {
  def notifyObservers()
  def registerObserver(observer: Updatable)
  def unregisterObserver(observer: Updatable)
}

// FIXME check trait Listeners in Akka!
// TODO Move to test directory when Akka impl is ready
trait ObservableDefImpl extends Observable {
  private[this] val observers = new CopyOnWriteArraySet[Updatable]

  def notifyObservers() {
    observers.toArray.toList.asInstanceOf[List[Observer]].foreach(_.update)
  }
  def registerObserver(observer: Updatable) {
    observers add observer
  }
  def unregisterObserver(observer: Updatable) {
    observers remove observer
  }
}

trait Updatable {
  def update()
}

/**
 * Object that gets notified when a given observable changes.
 */
trait Observer extends Updatable {
  def registerWith(observable: Observable)
  def unregisterWith(observable: Observable)
  def unregisterWithAll()
}

trait ObserverDefImpl extends Observer {
  // TODO Consider using actors!
  // FIXME check trait Listeners in Akka!
  private[this] val observables = new CopyOnWriteArraySet[Observable]

  def registerWith(observable: Observable) {
    observable registerObserver this
    observables add observable
  }
  def unregisterWith(observable: Observable) {
    observable unregisterObserver this
    observables remove observable
  }
  def unregisterWithAll() {
    observables.toArray.toList.asInstanceOf[List[Observable]].foreach(o => o unregisterObserver this)
    observables.clear()
  }
}

/**
 * Observable and assignable thread-safe proxy to concrete value.
 *
 * Observers can be registered with instances of this trait so that
 * they are notified when a different value is assigned to such instances.
 * Client code can copy the contained value or pass it to functions.
 */
trait ObservableValue[T] extends Observable {
  // FIXME consider moving to Agents or ScalaRX
  private[this] val valueRef: Ref[T] = Ref(initialValue)

  /** @return None by default, but do not hesitate to override it! */
  def initialValue(): T

  def apply() = valueRef.single()

  /**
   * @return The old value.
   * @note This method will notify observers of the value change if applicable.
   */
  def update(newValue: T): T = {
    val oldValue = valueRef.single.swap(newValue)
    if (oldValue != newValue) notifyObservers()
    oldValue
  }

  /** This method resets to the initial value '''without''' notifying observers. */
  def reset() { valueRef.single() = initialValue() }

  override def toString() = s"ObservableValue(now = ${valueRef.single()})"
}
