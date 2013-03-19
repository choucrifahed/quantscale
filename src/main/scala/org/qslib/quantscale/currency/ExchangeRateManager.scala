package org.qslib.quantscale.currency

import org.scala_tools.time.Imports._
import org.qslib.quantscale._
import scala.util.Try
import ExchangeRateType._
import java.util.concurrent.ConcurrentHashMap
import scala.util.Failure
import scala.util.Success

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
 Copyright (C) 2004, 2005 StatPro Italia srl
 Copyright (C) 2004 Decillion Pty(Ltd)

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
 * Exchange-rate repository.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
// FIXME consider using one date instead of 2 for storing a rate which results in a predictable behavior
trait ExchangeRateManager {

  /**
   * Adds an exchange rate. The given rate is valid between the given dates.
   *
   * '''Note''': If two rates are given between the same currencies and with overlapping
   * date ranges, the latest one added takes precedence during lookup.
   */
  def add(rate: ExchangeRate, startDate: LocalDate = MinDate, endDate: LocalDate = MaxDate)

  /**
   * Looks up the exchange rate between two currencies at a given
   * date.  If the given type is Direct, only direct exchange
   * rates will be returned if available; if Derived, direct
   * rates are still preferred but derived rates are allowed.
   *
   * '''WARNING''' if two or more exchange-rate chains are possible
   * which allow to specify a requested rate, it is unspecified which one
   * is returned.
   */
  def lookup(source: Currency,
    target: Currency,
    date: LocalDate = new LocalDate,
    erType: ExchangeRateType = Derived): Try[ExchangeRate]

  /**
   * Removes all added exchange rates.
   */
  def clear()
}

/**
 * Singleton implementation based on ConcurrentHashMap.
 */
// FIXME singletons don't play well with concurrent test suite execution (SBT)
object ExchangeRateManager extends ExchangeRateManager {

  // Beware this can be a memory leak if the map is never emptied.
  private val repository = new ConcurrentHashMap[String, Seq[Entry]]

  def add(rate: ExchangeRate, startDate: LocalDate, endDate: LocalDate) {
    val entry = Entry(rate, startDate, endDate)

    // Seq[Entry] is immutable but not repository this is why a synchronization is necessary
    repository.synchronized {
      val oldList = repository.get(entry.key)
      val newList = if (oldList == null) Seq(entry) else entry +: oldList
      repository.put(entry.key, newList)
    }
  }

  def clear() {
    repository.clear()
    addKnownRates()
  }

  def lookup(source: Currency,
    target: Currency,
    date: LocalDate = new LocalDate,
    erType: ExchangeRateType = Derived): Try[ExchangeRate] = {
    if (source == target) Success(ExchangeRate(source, target, 1.0))
    else if (erType == Direct) directLookup(source, target, date)
    else if (!source.triangulationCurrency.isEmpty) {
      val link = source.triangulationCurrency.get
      if (link == target) directLookup(source, link, date)
      else for {
        rate1 <- directLookup(source, link, date)
        rate2 <- lookup(link, target, date)
        chain <- rate1 chain rate2
      } yield chain
    } else if (!target.triangulationCurrency.isEmpty) {
      val link = target.triangulationCurrency.get
      if (source == link) directLookup(link, target, date)
      else for {
        rate1 <- lookup(source, link, date)
        rate2 <- directLookup(link, target, date)
        chain <- rate1 chain rate2
      } yield chain
    } else smartLookup(source, target, date)
  }

  private def smartLookup(source: Currency, target: Currency, date: LocalDate, forbidden: Seq[Int] = Seq()): Try[ExchangeRate] = {
    // Direct exchange rates are preferred.
    directLookup(source, target, date) recoverWith {
      case e: Throwable => {
        // If none is found, turn to smart lookup. The source currency
        // is forbidden to subsequent lookups in order to avoid cycles.
        val newForbidden = forbidden :+ source.numericCode
        val iterator = repository.entrySet().iterator()
        while (iterator.hasNext()) {
          val element = iterator.next()
          // we look for exchange-rate data which involve our source
          // currency...
          if (element.getKey().contains(source.code) && !element.getValue().isEmpty) {
            // ...whose other currency is not forbidden...
            val entry = element.getValue().head
            val other = if (source == entry.rate.source) entry.rate.target else entry.rate.source
            if (!newForbidden.contains(other.numericCode)) {
              // ...and which carries information for the requested date.
              // if we can get to the target from here...
              // ..we're done.
              val result = for {
                head <- directLookup(source, other, date)
                tail <- smartLookup(other, target, date, newForbidden)
                chain <- head chain tail
              } yield chain

              // This breaks the loop
              if (result.isSuccess) return result
            }
          }
        }

        // if the loop completed, we have no way to return the requested rate.
        Failure[ExchangeRate](new Exception("No conversion available from " + source + " to " + target + " on " + date))
      }
    }
  }

  private def directLookup(source: Currency, target: Currency, date: LocalDate): Try[ExchangeRate] = {
    val rates = repository.get(key(source, target))
    val filteredRates = if (rates != null) rates.filter(_.validAt(date)) else Seq()
    if (filteredRates.isEmpty) Failure(new Exception("No direct conversion available from " + source + " to " + target + " on " + date))
    else Success(filteredRates.head.rate)
  }

  private def key(source: Currency, target: Currency) = scala.collection.SortedSet(source.code, target.code).reduce(_ + _)

  private def addKnownRates() {
    // Currencies obsoleted by Euro
    add(ExchangeRate(EUR, Europe.ITL, 1936.27), new LocalDate(1999, 1, 1))
  }

  private case class Entry(rate: ExchangeRate, startDate: LocalDate = MinDate, endDate: LocalDate = MaxDate) {
    def key = ExchangeRateManager.key(rate.source, rate.target)
    def validAt(date: LocalDate) = date >= startDate && date <= endDate
  }

  addKnownRates()
}
