/*************************************************************************
*                                                                          
* main.cpp  -  description
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


#include <qapplication.h>
#include <qfont.h>
#include <qcdestyle.h>
#include <qplatinumstyle.h>
#include <qguardedptr.h>

#include "qmagicdisplay.h"
#include "splash.h"
#include "systempar.h"

int main(int argc, char *argv[])
{
  QGuardedPtr<Splash> splash;

  QApplication a(argc, argv);
  a.setFont(QFont("helvetica", 12));
  a.setStyle(new QCDEStyle());
  //a.setPalette( QPalette( QColor(100, 220, 200) ) );

  SystemPar systemINI;

  systemINI.readAll();

  qDebug("splash is: %d", systemINI.get_splash());
  qDebug("palette_path is: %s", systemINI.get_palette_path().c_str());

  QMAGICDisplay *qmagicdisplay = new QMAGICDisplay();
  a.setMainWidget(qmagicdisplay);
  qmagicdisplay->setCaption("File: <none>");
  qmagicdisplay->show();

  if ( systemINI.get_splash() )
    splash = new Splash(SPLASH_PIXMAP, SPLASH_TEXT);

  return a.exec();
}

// Local Variables:
// mode: c++
// End:
//EOF
