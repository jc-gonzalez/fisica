/////////////////////////////////////////////////////////////////
//
// period
//
//  Created: Thu May  7 16:24:22 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: 
//  Notes:   
//  
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt period.h}

/* @text
This section shows the include file {\tt period.h}
@endtext */

// @code
#ifndef __PERIOD_H__
#define __PERIOD_H__

#ifndef _this_
#define _this_ period
#endif
// @endcode

// @subsection Include files

// @code
#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

#include "jcmacros.h"
#include "jcdebug.h"

// @endcode

// @subsection Macro-definitions, and constants

// @code
#define COMMAND_LINE_OPTIONS    "hvqf:n:s:"
// @endcode

// @subsection Prototypes of functions

//++
// prototypes
//--

//@code

void present(void);
void log(const char *funct, char *fmt, ...);
void error(const char *funct, char *fmt, ...);
void usage(char *cmd);

// @endcode

// @code
#endif 
// @endcode
