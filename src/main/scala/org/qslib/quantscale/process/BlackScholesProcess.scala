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
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2004, 2005, 2006, 2007, 2009 StatPro Italia srl

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

package org.qslib.quantscale.process

import org.qslib.quantscale._
import org.qslib.quantscale.termstructure.YieldTermStructure
import org.qslib.quantscale.termstructure.volatility.equityfx.BlackVolTermStructure

/**
 * ==Generalized Black-Scholes Stochastic Process==
 * 
 * This trait describes the stochastic process governed by:
 * dS(t, S) = (r(t) - q(t) - sigma(t, S)^2 / 2) dt  + sigma dW(t)
 */
// FIXME Find a way to include math formulae in Scaladoc
trait GeneralizedBlackScholesProcess extends StochasticProcess1D {

  // TODO Check if this necessary
  def quoteX0(): Quote[Real]
  
  def dividendTS(): YieldTermStructure
  
  def riskFreeTS(): YieldTermStructure
  
  def blackVolTS(): BlackVolTermStructure
}
