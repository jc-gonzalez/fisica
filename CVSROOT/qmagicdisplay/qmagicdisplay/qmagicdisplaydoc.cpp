/*************************************************************************
*                                                                          
* qmagicdisplaydoc.cpp  -  description
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

#include <iostream.h>

#include <qdatastream.h>
#include <qfileinfo.h>

#include "qmagicdisplaydoc.h"

#include "datfilefactory.h"
#include "dateventheader.h"
#include "dateventdata.h"
#include "dateventpostdata.h"
#include "phefilefactory.h"
#include "pheeventheader.h"
#include "pheeventdata.h"
#include "pheeventpostdata.h"
#include "rflfilefactory.h"
#include "rfleventheader.h"
#include "rfleventdata.h"
#include "rfleventpostdata.h"


QMAGICDisplayDoc::QMAGICDisplayDoc() :
  modified(false), doctype(QMAGICDisplayDoc::DATDocType),
  ifile(0), data_source(0), evth(0), evtd(0), evtpd(0),
  ev_pos(0), total_events(0)
{
}

QMAGICDisplayDoc::~QMAGICDisplayDoc()
{
  if (ifile != 0) {
    ifile->close();
    delete ifile;
    ifile = 0;
  }

  if (ev_pos != 0) {
    delete ev_pos;
    ev_pos = 0;
  }

  if (data_source != 0) {
    delete data_source;
    data_source = 0;
  }
}

//void QMAGICDisplayDoc::newDoc()
//{
//}

bool QMAGICDisplayDoc::save()
{
  return true;
}

bool QMAGICDisplayDoc::saveAs(const QString &filename)
{
  return true;
}

bool QMAGICDisplayDoc::load(const QString &filename)
{
  qDebug("# QMAGICDisplayDoc::load(...)");

  ifile = new QFile( filename );
  CHECK_PTR( ifile );

  ifile->open( IO_ReadOnly );

  preReadDoc();

  emit documentChanged();
  slotReadEvent(1);

  qDebug("# QMAGICDisplayDoc::load(...) is done");
  return true;
}

bool QMAGICDisplayDoc::isModified() const
{
  qDebug("# QMAGICDisplayDoc::isModified(...)");

  return modified;
}

void QMAGICDisplayDoc::setDocType(EnumDocType t)
{
  qDebug("# QMAGICDisplayDoc::setDocType(...)");

  doctype = t;

  switch(doctype) {
  case DATDocType: MakeStructures(new DATFileFactory);
    break;
  case PHEDocType: MakeStructures(new PHEFileFactory);
    break;
  case RFLDocType: MakeStructures(new RFLFileFactory);
    break;
  }

  qDebug("Type of files to read is  doctype ->  %d = %s",
         doctype, ((doctype == DATDocType)?"DATDocType":
                   ((doctype == PHEDocType)?"PHEDocType":
                    ("RFLDocType"))));
}

int QMAGICDisplayDoc::getDocType()
{
  return doctype;
}

const char* QMAGICDisplayDoc::getDocName()
{
  QFileInfo fInfo( *ifile );
  //QString basename( fInfo.baseName() );
  //QString absname( fInfo.absFilePath() );
  return fInfo.filePath().latin1();
}

void QMAGICDisplayDoc::MakeStructures(FileFactory *f)
{
  qDebug("# QMAGICDisplayDoc::MakeStructures(...)");

  evth  = f->DoMakeEventHeader();
  evtd  = f->DoMakeEventData();
  evtpd = f->DoMakeEventPostData();

  CHECK_PTR( evth );
  CHECK_PTR( evtd );
  CHECK_PTR( evtpd );
}

void QMAGICDisplayDoc::preReadDoc()
{
  qDebug("# QMAGICDisplayDoc::preReadDoc(...)");

  if (ev_pos == 0)
    ev_pos = new QArray<int>(100);
  CHECK_PTR( ev_pos );

  if (data_source != 0)
    delete data_source;

  QDataStream* data_source = new QDataStream( &(*ifile) );
  CHECK_PTR( data_source );

  evth->setSource( data_source );
  evtd->setSource( data_source );
  evtpd->setSource( data_source );

  int i = 0;
  int current_size = ev_pos->size();

  do {

    if (((i+1) % 100) == 0)
      cout << ":";
    else if (((i+1) % 10) == 0)
      cout << ".";

    if (i == current_size) {
      current_size = int(current_size * 1.5);
      ev_pos->resize( current_size );
    }

    ev_pos->at(i) = ifile->at();

    evth->readEventHeader();
    evtd->readEventData();
    evtpd->readEventPostData();

    /*
      evth->printEventHeader();
      evtd->printEventData();
      evtpd->printEventPostData();
    */
    
    i++;

  } while (! data_source->eof());

  ev_pos->resize(i);

  cout << endl;
  
  total_events = i;
}

int QMAGICDisplayDoc::getTotalEvents()
{
  qDebug("# QMAGICDisplayDoc::getTotalEvents()");

  return total_events;
}

EventHeader& QMAGICDisplayDoc::getEvtH()
{
  return *evth;
}

EventData& QMAGICDisplayDoc::getEvtD()
{
  return *evtd;
}

EventPostData& QMAGICDisplayDoc::getEvtPD()
{
  return *evtpd;
}

void QMAGICDisplayDoc::slotReadEvent(int e)
{
  qDebug("# QMAGICDisplayDoc::slotReadEvent(%d)",e);

  if (static_cast<unsigned int>(e) <= ev_pos->size()) {
    // the indexes in the ev_pos table run from 0 to ev_pos->size()-1
    // while the value of our e here runs from 1 to ev_pos->size()
    // so we substract 1 when positioning the file pointer
    ifile->at( ev_pos->at(e-1) );

    evth->readEventHeader();
    evtd->readEventData();
    evtpd->readEventPostData();
  }
  
  emit eventChanged();
}

// Local Variables:
// mode: c++
// End:
//EOF
