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

import org.qslib.quantscale._
import org.scala_tools.time.Imports._

object exercise {

  val date1 = new LocalDate(2013, 1, 1)           //> date1  : org.joda.time.LocalDate = 2013-01-01
  val date2 = new LocalDate(2013, 2, 1)           //> date2  : org.joda.time.LocalDate = 2013-02-01

  // Test equality on European exercise

  val eurEx1 = EuropeanExercise(date1)            //> eurEx1  : org.qslib.quantscale.EuropeanExercise = EuropeanExercise(2013-01-
                                                  //| 01)
  val eurEx2 = EuropeanExercise(date1)            //> eurEx2  : org.qslib.quantscale.EuropeanExercise = EuropeanExercise(2013-01-
                                                  //| 01)
  val eurEx3 = EuropeanExercise(date2)            //> eurEx3  : org.qslib.quantscale.EuropeanExercise = EuropeanExercise(2013-02-
                                                  //| 01)

  eurEx1 == eurEx2                                //> res0: Boolean = true
  eurEx2 == eurEx3                                //> res1: Boolean = false

  // Test equality on American exercise

  val amEx1 = AmericanExercise(date1, date2, true)//> amEx1  : org.qslib.quantscale.AmericanExercise = AmericanExercise(2013-01-0
                                                  //| 1,2013-02-01,true)
  val amEx2 = AmericanExercise(date1, date2, false)
                                                  //> amEx2  : org.qslib.quantscale.AmericanExercise = AmericanExercise(2013-01-0
                                                  //| 1,2013-02-01,false)
  val amEx3 = AmericanExercise(date1, date2, true)//> amEx3  : org.qslib.quantscale.AmericanExercise = AmericanExercise(2013-01-0
                                                  //| 1,2013-02-01,true)

  amEx1 == amEx2                                  //> res2: Boolean = false
  amEx1 == amEx3                                  //> res3: Boolean = true

  // Test equality on Bermudan exercise

  val berEx1 = new BermudanExercise(Seq(date1, date2))
                                                  //> berEx1  : org.qslib.quantscale.BermudanExercise = BermudanExercise(List(201
                                                  //| 3-01-01, 2013-02-01))
  val berEx2 = new BermudanExercise(Seq(date2, date1))
                                                  //> berEx2  : org.qslib.quantscale.BermudanExercise = BermudanExercise(List(201
                                                  //| 3-01-01, 2013-02-01))
  berEx1 == berEx2                                //> res4: Boolean = true

  berEx1 match {
    case BermudanExercise(dates, _) => dates
  }                                               //> res5: Seq[org.scala_tools.time.Imports.LocalDate] = List(2013-01-01, 2013-0
                                                  //| 2-01)
}