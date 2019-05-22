/*************************************************************************
 *                                                                          
 * k2scanplotdoc.cpp  -  description
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


// include files for Qt
#include <qdir.h>
#include <qwidget.h>

// include files for KDE
#include <klocale.h>
#include <kmessagebox.h>
#include <kio/job.h>
#include <kio/netaccess.h>

// application specific includes
#include "k2scanplotdoc.h"
#include "k2scanplot.h"
#include "k2scanplotview.h"

QList<K2ScanPlotView> *K2ScanPlotDoc::pViewList = 0L;

K2ScanPlotDoc::K2ScanPlotDoc(QWidget *parent, const char *name) : QObject(parent, name)
{
  if(!pViewList)
  {
    pViewList = new QList<K2ScanPlotView>();
  }

  pViewList->setAutoDelete(true);
}

K2ScanPlotDoc::~K2ScanPlotDoc()
{
}

void K2ScanPlotDoc::addView(K2ScanPlotView *view)
{
  pViewList->append(view);
}

void K2ScanPlotDoc::removeView(K2ScanPlotView *view)
{
  pViewList->remove(view);
}
void K2ScanPlotDoc::setURL(const KURL &url)
{
  doc_url=url;
}

const KURL& K2ScanPlotDoc::URL() const
{
  return doc_url;
}

void K2ScanPlotDoc::slotUpdateAllViews(K2ScanPlotView *sender)
{
  K2ScanPlotView *w;
  if(pViewList)
  {
    for(w=pViewList->first(); w!=0; w=pViewList->next())
    {
      if(w!=sender)
        w->repaint();
    }
  }

}

bool K2ScanPlotDoc::saveModified()
{
  bool completed=true;

  if(modified)
  {
    K2ScanPlotApp *win=(K2ScanPlotApp *) parent();
    int want_save = KMessageBox::warningYesNoCancel(win, i18n("Warning"),
                                         i18n("The current file has been modified.\n"
                                              "Do you want to save it?"));
    switch(want_save)
    {
      case 1:
           if (doc_url.fileName() == i18n("Untitled"))
           {
             win->slotFileSaveAs();
           }
           else
           {
             saveDocument(URL());
       	   };

       	   deleteContents();
           completed=true;
           break;

      case 2:
           setModified(false);
           deleteContents();
           completed=true;
           break;	

      case 3:
           completed=false;
           break;

      default:
           completed=false;
           break;
    }
  }

  return completed;
}

void K2ScanPlotDoc::closeDocument()
{
  deleteContents();
}

bool K2ScanPlotDoc::newDocument()
{
  /////////////////////////////////////////////////
  // TODO: Add your document initialization code here
  /////////////////////////////////////////////////
  modified=false;
  doc_url.setFileName(i18n("Untitled"));

  return true;
}

bool K2ScanPlotDoc::openDocument(const KURL& url, const char *format /*=0*/)
{
  QString tmpfile;
  KIO::NetAccess::download( url, tmpfile );
  /////////////////////////////////////////////////
  // TODO: Add your document opening code here
  /////////////////////////////////////////////////

  KIO::NetAccess::removeTempFile( tmpfile );

  modified=false;
  return true;
}

bool K2ScanPlotDoc::saveDocument(const KURL& url, const char *format /*=0*/)
{
  /////////////////////////////////////////////////
  // TODO: Add your document saving code here
  /////////////////////////////////////////////////

  modified=false;
  return true;
}

void K2ScanPlotDoc::deleteContents()
{
  /////////////////////////////////////////////////
  // TODO: Add implementation to delete the document contents
  /////////////////////////////////////////////////

}
