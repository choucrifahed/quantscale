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
 Copyright (C) 2004, 2005, 2006 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2008 StatPro Italia srl

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

package org.qslib.quantscale.time

/**
 * Frequency of events.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
sealed trait Frequency {
  def apply(): Int
}

case object NoFrequency extends Frequency { override val apply = -1 }
case object Once extends Frequency { override val apply = 0 }
case object Annual extends Frequency { override val apply = 1 }
case object Semiannual extends Frequency { override val apply = 2 }
case object EveryFourthMonth extends Frequency { override val apply = 3 }
case object Quarterly extends Frequency { override val apply = 4 }
case object Bimonthly extends Frequency { override val apply = 6 }
case object Monthly extends Frequency { override val apply = 12 }
case object EveryFourthWeek extends Frequency { override val apply = 13 }
case object Biweekly extends Frequency { override val apply = 26 }
case object Weekly extends Frequency { override val apply = 52 }
case object Daily extends Frequency { override val apply = 365 }
case object OtherFrequency extends Frequency { override val apply = 999 }
