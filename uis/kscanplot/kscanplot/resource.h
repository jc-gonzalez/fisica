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


///////////////////////////////////////////////////////////////////
// COMMAND VALUES FOR MENUBAR AND TOOLBAR ENTRIES


///////////////////////////////////////////////////////////////////
// File-menu entries
#define ID_FILE_NEW                 10010
#define ID_FILE_OPEN                10020
#define ID_FILE_OPEN_RECENT         10030
#define ID_FILE_CLOSE               10040

#define ID_FILE_SAVE                10050
#define ID_FILE_SAVE_AS             10060

#define ID_FILE_PRINT               10070

#define ID_FILE_QUIT                10080

///////////////////////////////////////////////////////////////////
// Edit-menu entries
#define ID_EDIT_UNDO                11010
#define ID_EDIT_COPY                11020
#define ID_EDIT_CUT                 11030
#define ID_EDIT_PASTE               11040

///////////////////////////////////////////////////////////////////
// View-menu entries                    
#define ID_VIEW_TOOLBAR             12010
#define ID_VIEW_STATUSBAR           12020

///////////////////////////////////////////////////////////////////
// Window-menu entries
#define ID_WINDOW_NEW_WINDOW        13010
#define ID_WINDOW_CASCADE   		    13020
#define ID_WINDOW_TILE			        13030

///////////////////////////////////////////////////////////////////
// Help-menu entries
#define ID_HELP_CONTENTS            1002
#define ID_HELP_WHATS_THIS					1003
///////////////////////////////////////////////////////////////////
// General application values
#define ID_STATUS_MSG               1001

#endif // RESOURCE_H
