/////////////////////////////////////////////////////////////////
//
// corfilter
//
//  Created: Thu May  7 16:24:22 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: 
//  Notes:   
//  
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt corfilter.h}

// @text
// This section shows the include file {\tt corfilter.h}
// @endtext

// @code
#ifndef __CORFILTER_H__
#define __CORFILTER_H__

#ifndef _this_
#define _this_ corfilter
#endif
// @endcode

// @subsection Include files

// @code
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <unistd.h>

#include "jcmacros.h"
#include "jcdebug.h"

#include "COREventHeader.hxx"
#include "CORParticle.hxx"
#include "CORStatfile.hxx"
// @endcode

// @subsection Macro-definitions, and constants

// @code
#define COMMAND_LINE_OPTIONS    "f:l:u:m:vq"
#define BUFFER_LENGTH           5733
static char EVTH[] = "EVTH";

// @endcode

// @subsection Prototypes of functions

// @code

//++
// prototypes
//--

void present(void);
void clean(void);
void log(const char *funct, char *fmt, ...);
void error(const char *funct, char *fmt, ...);

// @endcode

// @code

#endif

// @endcode
