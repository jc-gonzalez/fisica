/*************************************************************************
*                                                                          
* camerapixels.h  -  description
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


#ifndef CAMERAPIXELS_H
#define CAMERAPIXELS_H

#include "camerabase.h"
#include "qmagicdisplaydoc.h"

#include <qarray.h>

/**
*@author J C Gonzalez
*/

class Pixel;
class Ellipsoid;
class QToolTipGroup;
class QPainter;
class Palette;
class QCanvasEllipse;

class CameraPixels : public CameraBase  {
  Q_OBJECT

public:
  CameraPixels(QMAGICDisplayDoc* d, EventData& ed, Palette& p,
               QWidget *parent=0, const char *name=0);
  ~CameraPixels();

public:
  void createPixels();
  void setImage(double a, double l, double w,
                double x, double y, double mx, double my);
  void printIt(QPainter *p);
  void contentsMousePressEvent(QMouseEvent* e);

public slots:
  void slotDisplayEvent(void);
  void slotToggleShowItem(int i);

private:
  QToolTipGroup*  tips;
  QArray<Pixel*>* pixelList;
  QCanvasText*    pixelInfo;
  EventData&      edata;
  Palette&        pal;
  Ellipsoid*      ellipsoid;

};

#endif

// Local Variables:
// mode: c++
// End:
//EOF
