/*************************************************************************
*                                                                          
* splash.h  -  description
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

/****************************************************************************
** Form interface generated from reading ui file 'splash.ui'
**
** Created: Fri Feb 23 11:52:33 2001
**      by:  The User Interface Compiler (uic)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/
#ifndef SPLASH_H
#define SPLASH_H

//#include "allsky_egret.xpm"

#include <qdialog.h>
class QVBoxLayout; 
class QHBoxLayout; 
class QGridLayout; 
class QTextView;
class QPixmap;
class QString;

class Splash : public QDialog
{ 
  Q_OBJECT

public:
  Splash( const char *pixmap_name,
          const char *the_text,                
          QWidget* parent = 0, const char* name = 0,
          bool modal = FALSE,
          WFlags fl = WType_Popup|WStyle_Tool|WStyle_StaysOnTop);
  ~Splash();

  QTextView* tvw;

};

#endif // SPLASH_H

// Local Variables:
// mode: c++
// End:
//EOF
