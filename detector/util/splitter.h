/////////////////////////////////////////////////////////////////
//
// splitter
//
//  Created: Thu May  7 16:24:22 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Program for splitter simulation
//  Notes:   
//  
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt splitter.h}

/* @text
This section shows the include file {\tt splitter.h}
@endtext */

// @code
#ifndef __SPLITTER_H__
#define __SPLITTER_H__

#ifndef _this_
#define _this_ splitter
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

#include "jcmacros.h"
#include "jcdebug.h"

#include "COREventHeader.hxx"
#include "CORParticle.hxx"
#include "CORStatfile.hxx"

extern "C" {

#include "ranlib.h"

}

// @endcode

// @subsection Macro-definitions, and constants

// @code
#define COMMAND_LINE_OPTIONS    "f:p:vq"
#define BLOCK_LENGTH            5733
#define SUBBLOCK_LENGTH         273
#define NUM_SUBBLOCKS           21
#define NUM_CPHOTONS            39
#define MARK_LENGTH             4
// @endcode

// @subsection Prototypes of functions

//++
// prototypes
//--

//@code

void present(void);
void log(const char *funct, char *fmt, ...);
void error(const char *funct, char *fmt, ...);
static void read_mark(ifstream &f);
static void read_block(ifstream &f, float * blk);
static void write_subblock(ofstream &f, float * blk);

// @endcode

// @code
#endif 
// @endcode
