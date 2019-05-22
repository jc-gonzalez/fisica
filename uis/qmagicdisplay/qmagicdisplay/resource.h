/*************************************************************************
*                                                                          
* resource.h  -  description
*
* Copyright (C) 2001  J C Gonzalez
* gonzalez@gae.ucm.es
*
*------------------------------------------------------------------------
*
* Copyright (C) 2001 J C Gonzalez
*  
* This program is free software;  you can redistribute it and/or  modify
* it under the terms  of the GNU General  Public License as published by
* the Free Software Foundation; either version  2 of the License, or (at
* your option) any later version.
* 
* This piece of code is distributed in the hope  that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS
* FOR A PARTICULAR PURPOSE.
* 
* In no  event shall his author  be liable for  any special, incidental,
* indirect  or  consequential  damages  of any  kind,   or  any  damages
* whatsoever resulting from loss of use, data or profits, whether or not
* advised of the possibility of damage, and on  any theory of liability,
* arising out  of or in connection  with the use  or performance of this
* software. You've been warned.
* 
************************************************************************/

#ifndef RESOURCE_H
#define RESOURCE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


///////////////////////////////////////////////////////////////////
// resource.h  -- contains macros used for commands

#define CONVERT_TO_STRING(x)   #x
#define STRING(x)   CONVERT_TO_STRING(x)

#define cVERSION  STRING( VERSION )

///////////////////////////////////////////////////////////////////
// COMMAND VALUES FOR MENUBAR AND TOOLBAR ENTRIES


///////////////////////////////////////////////////////////////////
// File-menu entries
#define ID_FILE_NEW                 10020
#define ID_FILE_OPEN                10030

#define ID_FILE_SAVE                10050
#define ID_FILE_SAVE_AS             10060
#define ID_FILE_CLOSE               10070

#define ID_FILE_PRINT               10080

#define ID_FILE_QUIT                10100


///////////////////////////////////////////////////////////////////
// Edit-menu entries
#define ID_EDIT_UNDO                11010
#define ID_EDIT_REDO                11020
#define ID_EDIT_COPY                11030
#define ID_EDIT_CUT                 11040
#define ID_EDIT_PASTE               11050
#define ID_EDIT_SELECT_ALL          11060


///////////////////////////////////////////////////////////////////
// View-menu entries                    
#define ID_VIEW_TOOLBAR             12010
#define ID_VIEW_STATUSBAR           12020

///////////////////////////////////////////////////////////////////
// Opt-menu entries
#define ID_OPT_READDAT              13010
#define ID_OPT_READPHE              13020
#define ID_OPT_READRFL              13030

///////////////////////////////////////////////////////////////////
// Help-menu entries
#define ID_HELP_ABOUT               1002

///////////////////////////////////////////////////////////////////
// General application values
#define IDS_APP_ABOUT       "QMAGICDisplay\nVersion " VERSION \
"\n\nEvent Display for MAGIC MonteCarlo data\n\n" \
"Copyright (c) 2001 by J C Gonzalez\n" \
"All rights reserved\n\n" \
"This program is free software;  you can\n" \
"redistribute it and/or  modify it under the\n" \
"terms of the GNU General Public License as\n" \
"published by the Free Software Foundation;\n" \
"either version 2 of the License, or (at\n" \
"your option) any later version."

#define IDS_STATUS_DEFAULT  "Ready."

#define MSG_ERRORSETMODE    "Cannot set the reading file mode when a\n"\
"file is already opened. Close it first."

#define SPLASH_PIXMAP "allsky_egret.xpm"
//#define SPLASH_PIXMAP  (const char**)allsky_egret_xpm

#define SPLASH_TEXT \
"<b><font color=blue size=+2>QMAGICD</font>"\
"<font color=blue size=+1>ISPLAY   Version   </font>"\
"<font color=red size=+2>" VERSION "</font></b>"\
"<br><hr>"\
"Copyright &copy; 2000,2001  J C Gonzalez<br>"\
"All rights reserved<br><br><br>"\
"This program is free software; you can redistribute it <br>"\
"and/or modify it under the terms of the GNU General<br>"\
"Public License as published by the Free Software <br>"\
"Foundation; either version 2 of the License, or (at your <br>"\
"option) any later version."

#endif // RESOURCE_H

// Local Variables:
// mode: c++
// End:
//EOF
