/*************************************************************************
*                                                                          
* camerabase.cpp  -  description
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


#include "camerabase.h"

#include "qmagicdisplaydoc.h"

CameraBase::CameraBase(QMAGICDisplayDoc* d,
                       QWidget *parent, const char *name ) :
  QCanvasView(0,parent,name)
{
  QCanvas *c;
  c = new QCanvas(500, 500);
  CHECK_PTR( c );

  setCanvas( c );
  c->setBackgroundColor( QColor(0,40,80) );
  setFixedSize( QSize(502, 502) );

  doc = d;
}

CameraBase::~CameraBase() { }

QSize CameraBase::sizeHint() const
{
  return QSize(510, 510);
}

void CameraBase::createPixels() { }

void CameraBase::printIt(QPainter *) { }

void CameraBase::contentsMousePressEvent(QMouseEvent* ) { }

void CameraBase::slotDisplayEvent(void) { }

// Local Variables:
// mode: c++
// End:
//EOF
