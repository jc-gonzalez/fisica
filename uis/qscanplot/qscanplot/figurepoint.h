/*************************************************************************
 *                                                                          
 * figurepoint.h  -  description
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


#ifndef FIGUREPOINT_H
#define FIGUREPOINT_H

#include <qcanvas.h>

#include "point.h"
#include "gpoint.h"

#include <qcolor.h>
#include <qbrush.h>
#include <qpen.h>

/**
  *@author J C Gonzalez
  */

static const int FigurePointRTTI = 9800;

class FigurePoint : public QCanvasPolygon  {
    
public: 
	FigurePoint(QCanvas* c=0, int len=5);
	~FigurePoint();
	
public:
	inline void setX(double nx) { p.setX( nx ); }
	inline void setY(double ny) { p.setY( ny ); }
	inline double getX() { return p.getX(); }
	inline double getY() { return p.getY(); }
	
	inline void setGX(double nx) { gp.setX( nx ); }
	inline void setGY(double ny) { gp.setY( ny ); }
	inline double getGX() { return gp.getX(); }
	inline double getGY() { return gp.getY(); }

  /** Re-implemented from QCanvasPolygon. Draws the point */
  void drawShape ( QPainter & p );
	
  int rtti () const { return FigurePointRTTI; }

 private:
  Point        p;
  GPoint       gp;

};

#endif
