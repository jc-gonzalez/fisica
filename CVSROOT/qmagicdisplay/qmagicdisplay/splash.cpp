/*************************************************************************
*                                                                          
* splash.cpp  -  description
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

/****************************************************************************
** Form implementation generated from reading ui file 'splash.ui'
**
** Created: Fri Feb 23 11:52:33 2001
**      by:  The User Interface Compiler (uic)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/
#include "splash.h"

#include <qtextview.h>
#include <qpixmap.h>
#include <qbrush.h>
#include <qtimer.h>
#include <qfont.h>

/* 
*  Constructs a Splash which is a child of 'parent', with the 
*  name 'name' and widget flags set to 'f' 
*
*  The dialog will by default be modeless, unless you set 'modal' to
*  TRUE to construct a modal dialog.
*/
Splash::Splash( const char *pixmap_name,
                const char *the_text,
                QWidget* parent,  const char* name,
                bool modal, WFlags fl )
  : QDialog( parent, name, modal, fl )
{
  if ( !name )
    setName( "Splash" );

  tvw = new QTextView( this );

  QBrush paper;
  paper.setPixmap( QPixmap(pixmap_name) );
  tvw->setPaper( paper );
  tvw->setGeometry( 2, 2,
                    paper.pixmap()->width(),
                    paper.pixmap()->height() );
  tvw->setFrameShape( QTextView::NoFrame );
  tvw->setTextFormat( QTextView::RichText );
  tvw->setFont(QFont("helvetica", 10));
  tvw->setText( the_text );

  resize( paper.pixmap()->width() + 4,
          paper.pixmap()->height() + 4 );

  //setCaption( tr( "Welcome to QMAGICDisplay v. 0.1" ) );

  show();

  QTimer::singleShot(3 * 1000, this, SLOT(close()));
}

/*  
*  Destroys the object and frees any allocated resources
*/
Splash::~Splash()
{
  // no need to delete child widgets, Qt does it all for us
}


// Local Variables:
// mode: c++
// End:
//EOF
