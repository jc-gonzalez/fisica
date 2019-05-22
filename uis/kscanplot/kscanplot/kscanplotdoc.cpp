/*************************************************************************
 *                                                                          
 * kscanplotdoc.cpp  -  description
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
#include <qfileinfo.h>
#include <qwidget.h>

// include files for KDE
#include <klocale.h>
#include <kmessagebox.h>
#include <kfiledialog.h>

// application specific includes
#include "kscanplotdoc.h"
#include "kscanplot.h"
#include "kscanplotview.h"


KScanPlotDoc::KScanPlotDoc()
{
  pViewList = new QList<KScanPlotView>;
  pViewList->setAutoDelete(false);
}

KScanPlotDoc::~KScanPlotDoc()
{
  delete pViewList;
}

void KScanPlotDoc::addView(KScanPlotView *view)
{
  pViewList->append(view);
	changedViewList();
}

void KScanPlotDoc::removeView(KScanPlotView *view)
{
	  pViewList->remove(view);
	  if(!pViewList->isEmpty())
			changedViewList();
		else
			deleteContents();
}

void KScanPlotDoc::changedViewList(){	
	
	KScanPlotView *w;
	if((int)pViewList->count() == 1){
  	w=pViewList->first();
  	w->setCaption(m_title);
	}
	else{	
		int i;
    for( i=1,w=pViewList->first(); w!=0; i++, w=pViewList->next())
  		w->setCaption(QString(m_title+":%1").arg(i));	
	}
}

bool KScanPlotDoc::isLastView() {
  return ((int) pViewList->count() == 1);
}


void KScanPlotDoc::updateAllViews(KScanPlotView *sender)
{
  KScanPlotView *w;
  for(w=pViewList->first(); w!=0; w=pViewList->next())
  {
     w->update(sender);
  }

}

void KScanPlotDoc::setPathName(const QString &name)
{
  m_filename=name;
	m_title=QFileInfo(name).fileName();
}

const QString& KScanPlotDoc::pathName() const
{
  return m_filename;
}

void KScanPlotDoc::setTitle(const QString &title)
{
  m_title=title;
}

const QString &KScanPlotDoc::title() const
{
  return m_title;
}


void KScanPlotDoc::closeDocument()
{
  KScanPlotView *w;
  if(!isLastView())
  {
    for(w=pViewList->first(); w!=0; w=pViewList->next())
    {
   	 	if(!w->close())
 				break;
    }
	}
  if(isLastView())
  {
  	w=pViewList->first();
  	w->close();
  }
}

bool KScanPlotDoc::newDocument()
{
  /////////////////////////////////////////////////
  // TODO: Add your document initialization code here
  /////////////////////////////////////////////////
  modified=false;
  return true;
}

bool KScanPlotDoc::openDocument(const QString &filename, const char *format /*=0*/)
{

	QFile f( filename );
	if ( !f.open( IO_ReadOnly ) )
		return false;
  /////////////////////////////////////////////////
  // TODO: Add your document opening code here
  /////////////////////////////////////////////////
	f.close();
	
  modified=false;
  m_filename=filename;
	m_title=QFileInfo(f).fileName();
  return true;
}

bool KScanPlotDoc::saveDocument(const QString &filename, const char *format /*=0*/)
{
	QFile f( filename );
	if ( !f.open( IO_WriteOnly ) )
		return false;

  /////////////////////////////////////////////////
  // TODO: Add your document saving code here
  /////////////////////////////////////////////////

  f.close();

  modified=false;
  m_filename=filename;
	m_title=QFileInfo(f).fileName();
  return true;
}

void KScanPlotDoc::deleteContents()
{
  /////////////////////////////////////////////////
  // TODO: Add implementation to delete the document contents
  /////////////////////////////////////////////////

}

bool KScanPlotDoc::canCloseFrame(KScanPlotView* pFrame)
{
	if(!isLastView())
		return true;
		
	bool ret=false;
  if(isModified())
  {
		QString saveName;
  	switch(KMessageBox::warningYesNoCancel(pFrame, i18n("The current file has been modified.\n"
                          "Do you want to save it?"),title()))
    {
			case KMessageBox::Yes:
				if(title().contains(i18n("Untitled")))
				{
					saveName=KFileDialog::getSaveFileName(QDir::currentDirPath(),
                      i18n("*|All files"), pFrame, i18n("Save as..."));
          if(saveName.isEmpty())
          	return false;
				}
				else
					saveName=pathName();
					
				if(!saveDocument(saveName))
				{
 					switch(KMessageBox::warningYesNo(pFrame,i18n("Could not save the current document !\n"
																												"Close anyway ?"), i18n("I/O Error !")))
 					{
 						case KMessageBox::Yes:
 							ret=true;
 						case KMessageBox::No:
 							ret=false;
 					}	        			
				}
				else
					ret=true;
				break;
			case KMessageBox::No:
				ret=true;
				break;
			case KMessageBox::Cancel:
			default:
				ret=false; 				
				break;
		}
	}
	else
		ret=true;
		
	return ret;
}
