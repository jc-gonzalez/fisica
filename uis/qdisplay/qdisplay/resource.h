/***************************************************************************
                          resource.h  -  description
                             -------------------
    begin                : mié ene 12 22:01:32 CET 2000
    copyright            : (C) 2000 by Jose Carlos Gonzalez
    email                : gonzalez@mppmu.mpg.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
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
#define ID_FILE_OPEN                10010
#define ID_FILE_SAVE_SELECTED       10020
#define ID_FILE_CLOSE               10030

#define ID_FILE_PRINT               10040

#define ID_FILE_QUIT                10100


///////////////////////////////////////////////////////////////////
// View-menu entries                    
#define ID_VIEW_TOOLBAR             12010
#define ID_VIEW_STATUSBAR           12020
#define ID_VIEW_PALETTE             12030


///////////////////////////////////////////////////////////////////
// Options-menu entries
#define ID_OPT_PRINT_IN_COLOR       13010
#define ID_OPT_PRINTOPT             13020
#define ID_OPT_DISPLAY_SIGNAL       13030
#define ID_OPT_DISPLAY_TIMES        13040


///////////////////////////////////////////////////////////////////
// Help-menu entries
#define ID_HELP_ABOUT               1002


///////////////////////////////////////////////////////////////////
// General application values
#define IDS_VERSION                "Version " VERSION
#define IDS_APP_ABOUT               "QDisplay\n" IDS_VERSION \
                                    "\nKopyleft (K) 2000 by Jose Carlos Gonzalez"
#define IDS_STATUS_DEFAULT          "Ready."

#endif // RESOURCE_H
