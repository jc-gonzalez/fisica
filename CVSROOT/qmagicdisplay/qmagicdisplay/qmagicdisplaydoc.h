/*************************************************************************
*                                                                          
* qmagicdisplaydoc.h  -  description
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

#ifndef QMAGICDISPLAYDOC_H
#define QMAGICDISPLAYDOC_H

// include files for QT
#include <qobject.h>

// application specific includes
#include "filefactory.h"
#include "eventheader.h"
#include "eventdata.h"
#include "eventpostdata.h"

#include <qfile.h>

#include <qarray.h>

/**
* the Document Class
*/

class QDataStream;

class QMAGICDisplayDoc : public QObject
{
  Q_OBJECT

public:
  typedef enum {DATDocType, PHEDocType, RFLDocType} EnumDocType;

public:
  QMAGICDisplayDoc();
  ~QMAGICDisplayDoc();
  //void newDoc();
  bool save();
  bool saveAs(const QString &filename);
  bool load(const QString &filename);
  bool isModified() const;
  void setDocType(EnumDocType t);
  int  getDocType();
  const char* getDocName();

  void MakeStructures(FileFactory* f);
  void preReadDoc();

  EventHeader&    getEvtH();
  EventData&      getEvtD();
  EventPostData&  getEvtPD();

  int getTotalEvents();
    
public slots:
  void slotReadEvent(int e);

signals:
  void documentChanged();
  void eventChanged();

protected:
  bool            modified;
  EnumDocType     doctype;

private:
  QFile*          ifile;
  QDataStream*    data_source;
  EventHeader*    evth;
  EventData*      evtd;
  EventPostData*  evtpd;
  QArray<int>*    ev_pos;
  int             total_events;
};

#endif

// Local Variables:
// mode: c++
// End:
//EOF
