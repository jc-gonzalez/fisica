/*************************************************************************
*                                                                          
* pixelbase.h  -  description
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


#ifndef PIXELBASE_H
#define PIXELBASE_H

#include <qcanvas.h>
//#include <qrect.h>
//#include <qpointarray.h>

#include "autointerface.h"

/**
*@author J C Gonzalez
*/


class CameraPixels;

class PixelBase : public QCanvasPolygon  {
  // Q_OBJECT

public:
  PixelBase(int n=0, QCanvas *parent=0);
  ~PixelBase();

public:
  virtual void buildPixel();
  virtual void buildPixel(int n);

  void setWorldTransform(QWMatrix *m);
  void setColor(QColor &c);

  DECLARE_INTERFACE_VIRTUAL(PixelBase,int,id);
  DECLARE_INTERFACE_VIRTUAL(PixelBase,double,zoom);
  DECLARE_INTERFACE_VIRTUAL(PixelBase,double,charge);
  DECLARE_INTERFACE_VIRTUAL(PixelBase,double,time);

  virtual int get_size();

protected:
  virtual void drawShape(QPainter &p);
  virtual void mousePressEvent(QMouseEvent * e);

private:
  int          id;
  double       zoom;
  double       charge;
  double       time;
  int          size;

//  QPoint*      center;
//  QPointArray* vertex;
  QWMatrix*    inverse_world_transform;
};

#endif

// Local Variables:
// mode: c++
// End:
//EOF
