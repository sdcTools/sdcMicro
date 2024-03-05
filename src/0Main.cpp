/**
 * RankSwapping
 *
 * copyright: Organisation For Economic Co-Operation And Development
 *
 *  This program is free software; you can redistribute it and/or modify it under the terms of the
 *  GNU Lesser General Public License as published by the Free Software Foundation; either version
 *  2.1 of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *  See the GNU Lesser General Public License for more details.
 *
 *  The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
 */

#include <Rcpp.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <stdio.h>
#include <float.h>
#include <limits.h>
#include <time.h>
#if !defined(__APPLE__)
#include <malloc.h>
#endif
//#if defined(__APPLE__)
//#if (__APPLE_CC__<5658)
//#include <malloc.h>
//#endif
//#endif

#include "Framework.h"

// //RankSwapping
enum{
	 es_NbHashBit	=	16,
	 es_NbHashList	=	1 << es_NbHashBit
 };

#include "RankSwapping.h"


//Mdav
//enum{
//   es_NbHashBitXX  = 16,
//   es_NbHashListXX = 1 << es_NbHashBitXX
// };
//#include "Mdav.h"

//Suda2
#include "Suda2.h"

//#include "Pram.h"

#include "Measure_Risk.h"
#include "Measure_Hierarchical.h"
#include "Measure_Threshold.h"
