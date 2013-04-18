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
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2006 Piter Dias

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
 * ==Business Day Conventions==
 * These conventions specify the algorithm used to adjust a date in case
 * it is not a valid business day.
 *
 * @author Choucri FAHED
 * @since 1.0
 */
sealed trait BusinessDayConvention

/** Choose the first business day after the given holiday. (ISDA) */
case object Following extends BusinessDayConvention

/**
 * Choose the first business day after
 * the given holiday unless it belongs
 * to a different month, in which case
 * choose the first business day before
 * the holiday. (ISDA)
 */
case object ModifiedFollowing extends BusinessDayConvention

/** Choose the first business day before the given holiday. (ISDA) */
case object Preceding extends BusinessDayConvention

/**
 * Choose the first business day before
 * the given holiday unless it belongs
 * to a different month, in which case
 * choose the first business day after
 * the holiday. (NON ISDA)
 */
case object ModifiedPreceding extends BusinessDayConvention

/** Do not adjust. (NON ISDA) */
case object Unadjusted extends BusinessDayConvention
