/*************************************************************************
*                                                                          
* ellipsoid.h  -  description
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


#ifndef ELLIPSOID_H
#define ELLIPSOID_H

#include <qcanvas.h>

/**
*@author J C Gonzalez
*/

class Ellipsoid {
  //  Q_OBJECT
public: 
  Ellipsoid(QCanvas* c);
  ~Ellipsoid();

  void toggleEllipse();
  void toggleAxes();
  void toggleCentroid();
  void toggleMaximum();

  void setZoom(double z);
  void setCentroid(double x, double y);
  void setMaximum(double x, double y);
  void setShape(double l, double w);
  void setAlpha(double a);
  
  void draw();
  
  void setWorldTransform(QWMatrix *m);

private:
  bool show_ellipse;
  bool show_axes;
  bool show_centroid;
  bool show_maximum;

  double zoom;
  double zoom_1000;
  double zoom_deg;
  double zoom_half;
  double postzoom_deg;
  double halfside;

  double cx, cy;
  double mx, my;
  double length, width;
  double alpha;

  QCanvasEllipse*   ellipse0;
  QCanvasPolygon*   ellipse;
  QCanvasLine*      axisMain;
  QCanvasLine*      axisSecondary;
  QCanvasRectangle* centroid;
  QCanvasRectangle* maximum;

  QPointArray*      pts;

  QWMatrix* inverseWorldTransform;
  QWMatrix* applyScale_and_Alpha;
  QWMatrix* totalTransform;

};

#endif

// Local Variables:
// mode: c++
// End:
//EOF
