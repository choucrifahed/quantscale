package org.qslib.quantscale.pattern

import java.util.concurrent.CopyOnWriteArraySet

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

/**
 * Object that notifies its changes to a set of observers.
 */
trait Observable {
  def notifyObservers()
  def registerObserver(observer: Observer)
  def unregisterObserver(observer: Observer)
}

// FIXME check trait Listeners in Akka!
// TODO Move to test directory when Akka impl is ready
trait ObservableDefImpl extends Observable {
  private val observers = new CopyOnWriteArraySet[Observer]

  def notifyObservers() {
    observers.toArray.toList.asInstanceOf[List[Observer]].foreach(_.update)
  }
  def registerObserver(observer: Observer) {
    observers add observer
  }
  def unregisterObserver(observer: Observer) {
    observers remove observer
  }
}

/**
 * Object that gets notified when a given observable changes.
 */

trait Observer {
  def update()
  def registerWith(observable: Observable)
  def unregisterWith(observable: Observable)
  def unregisterWithAll()
}

trait ObserverDefImpl extends Observer {
  // TODO Consider using actors!
  // FIXME check trait Listeners in Akka!
  private val observables = new CopyOnWriteArraySet[Observable]

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
