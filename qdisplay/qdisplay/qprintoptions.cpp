/***************************************************************************
                          qprintoptions.cpp  -  description
                             -------------------
    begin                : Fri Oct 13 2000
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
#include "qprintoptions.h"

QPrintOptions::QPrintOptions(QWidget *parent, const char *name)
  : QDialog(parent,name,true)
{
	initDialog();
	
	printOutputTo = 0;
	
	connect(btnCancel, SIGNAL(clicked()), SLOT(reject()));
	connect(btnOK, SIGNAL(clicked()), SLOT(accept()));
  connect(btngrpOutput, SIGNAL(clicked(int)), SLOT(set_PrintOutputTo(int)) );
}

QPrintOptions::~QPrintOptions()
{
}

void QPrintOptions::set_PrintInColor(bool b)
{ chkbxPrintInColor->setChecked(b); }

bool QPrintOptions::get_PrintInColor()
{ return chkbxPrintInColor->isChecked(); }

bool QPrintOptions::get_PrintImage()
{ return chkbxPrintImage->isChecked(); }

bool QPrintOptions::get_PrintStat()
{ return chkbxPrintStatistics->isChecked(); }

bool QPrintOptions::get_PrintHist()
{ return chkbxPrintHistograms->isChecked(); }

const char * QPrintOptions::get_PrintCommand()
{ return lePrintCommand->text(); }

const char * QPrintOptions::get_PrintFilePrefix()
{ return lePrintPrefix->text(); }

const char * QPrintOptions::get_PrintFileName()
{ return lePrintFilename->text(); }

int QPrintOptions::get_PrintOutputTo()
{ return printOutputTo; }

void QPrintOptions::set_PrintOutputTo(int i)
{ printOutputTo = i; }
