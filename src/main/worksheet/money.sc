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

import com.github.nscala_time.time.Imports._
import org.qslib.quantscale._
import Implicits._

/**
 * Examples of using Money classes.
 */
object money {
  val eur_usd = ExchangeRate(EUR, USD, 1.2042)    //> eur_usd  : org.qslib.quantscale.ExchangeRate = ExchangeRate(EUR,USD,1.2042,
                                                  //| Direct)

  val m1 = 50000.0 * EUR                          //> m1  : org.qslib.quantscale.Money = 50000.0 �
  val m2 = 100000.0 * USD                         //> m2  : org.qslib.quantscale.Money = 100000.0 $

  val conversionType = NoConversion               //> conversionType  : org.qslib.quantscale.NoConversion.type = NoConversion

  val calculated = eur_usd.exchange(m1).get       //> calculated  : org.qslib.quantscale.Money = 60210.0 $
  val expected = (m1.value * eur_usd.rate) * USD  //> expected  : org.qslib.quantscale.Money = 60210.0 $

  calculated == expected                          //> res0: Boolean = true
  calculated ~= expected                          //> res1: Boolean = true

  val date1 = new LocalDate(2013, 1, 1)           //> date1  : org.joda.time.LocalDate = 2013-01-01

  val scf = SimpleCashFlow(date1, 100 * EUR)      //> scf  : org.qslib.quantscale.SimpleCashFlow = SimpleCashFlow(date=2013-01-01
                                                  //| , amount=100.0 �)

}