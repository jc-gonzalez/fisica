/***************************************************************************
                          main.cpp  -  description
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

#include <qapplication.h>
#include <qfont.h>

#include <qplatinumstyle.h>

#include "qdisplay.h"

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);

  a.setStyle( new QPlatinumStyle() );

  /* set here the font you want, although helvetica-12 is OK */
  a.setFont(QFont("helvetica", 12));

  /* uncomment the following line, if you want a Windows 95 look*/
  //a.setStyle(WindowsStyle);
    
  QDisplay *qdisplay=new QDisplay();
  a.setMainWidget(qdisplay);

  qdisplay->setCaption("File: <none>");
  qdisplay->setFixedSize( 700, 600 );
  qdisplay->show();

  return a.exec();
}
