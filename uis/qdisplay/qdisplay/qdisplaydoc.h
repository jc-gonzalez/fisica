/***************************************************************************
                          qdisplaydoc.h  -  description
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
#ifndef QDISPLAYDOC_H
#define QDISPLAYDOC_H

// include files for QT
#include <qobject.h>
#include <qfile.h>

#include "infinitearray.h"

// application specific includes

/**
  * the Document Class
  */

class QDisplayDoc : public QObject
{
  Q_OBJECT

  public:
    QDisplayDoc();
    ~QDisplayDoc();
    bool    saveSelected(const QString &filename);
    bool    load(const QString &filename);
    bool    close();
    bool    isModified() const;

    const InfiniteArray<int> * ptr_evt() const;
   	const InfiniteArray<int> * ptr_evtToSave() const;
    int                  val_nEvents() const;
    QFile *              ptr_dataFile() const;

  signals:
    void documentChanged();

  protected:
    bool modified;

  private:
  	InfiniteArray<int> evt;
  	InfiniteArray<int> evtToSave;
  	
  	int nEvents;

		QFile *dataFile;
		
		void readAllEvents();
		
};

#endif
