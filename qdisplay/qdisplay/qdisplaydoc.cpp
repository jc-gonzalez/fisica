/***************************************************************************
                          qdisplaydoc.cpp  -  description
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

#include "qdisplaydoc.h"

QDisplayDoc::QDisplayDoc()
{
  modified = false;
  dataFile = 0;
}

QDisplayDoc::~QDisplayDoc()
{
}

bool QDisplayDoc::saveSelected(const QString &filename)
{
  return true;
}

bool QDisplayDoc::close(void)
{
	if ( ( dataFile != 0 ) && ( dataFile->isOpen() ) ) {
		dataFile->close();
		delete dataFile;
		dataFile = 0;
	}
	
	/* initialize variable with total number of events */
	nEvents = 0;
	evt.clear();
	evtToSave.clear();
	
  emit documentChanged();
  return true;
}

bool QDisplayDoc::load(const QString &filename)
{
	close();
	
	dataFile = new QFile( filename );
	dataFile->open( IO_ReadOnly );
	
	readAllEvents();
	
  emit documentChanged();
  return true;
}

bool QDisplayDoc::isModified() const
{
  return modified;
}

int QDisplayDoc::val_nEvents() const
{
  return nEvents;
}

const InfiniteArray<int> * QDisplayDoc::ptr_evt() const
{
  return &evt;
}

const InfiniteArray<int> * QDisplayDoc::ptr_evtToSave() const
{
  return &evtToSave;
}

QFile * QDisplayDoc::ptr_dataFile() const
{
  return dataFile;
}


void QDisplayDoc::readAllEvents()
{
	char wholeLine[20000];
	
	// cout << "I'm in QDisplayDoc::readAllEvents()" << endl << flush;
	
	/* loop while still there are data in the file */
	while ( ! dataFile->atEnd() ) {
	
		evt.append( dataFile->at() );
		evtToSave.append( 0 );
		
		dataFile->readLine( wholeLine, 20000 );
		dataFile->readLine( wholeLine, 20000 );
		dataFile->readLine( wholeLine, 20000 );
		
		nEvents++;
		
		// cout << "Event " << nEvents
		//      << " starts at pos " << evt[nEvents-1] << endl << flush;
	
	}
	
}
