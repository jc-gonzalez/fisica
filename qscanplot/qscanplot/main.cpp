/***************************************************************************
                          main.cpp  -  description
                             -------------------
    begin                : Fri Oct 13 19:38:27 CEST 2000
    copyright            : (C) 2000 by J C Gonzalez
    email                : gonzalez@gae.ucm.es
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

#include "qscanplot.h"

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);
  a.setFont(QFont("helvetica", 12));
  /* uncomment the following line, if you want a Windows 95 look*/
  // a.setStyle(WindowsStyle);
    
  QScanPlot *qscanplot=new QScanPlot();
  a.setMainWidget(qscanplot);

  qscanplot->setCaption("Document 1");
  qscanplot->show();

  return a.exec();
}
