/*************************************************************************
 *                                                                          
 * figurepoint.cpp  -  description
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


#include "figurepoint.h"

#include <qpainter.h>

FigurePoint::FigurePoint(QCanvas *c) : QCanvasPolygon(c)
{
  QPointArray p(12);
  p.setPoint( 0, -5,  0);
  p.setPoint( 1, -2,  0);
  p.setPoint( 2,  0,  2);
  p.setPoint( 3,  0,  5);
  p.setPoint( 4,  0,  2);
  p.setPoint( 5,  2,  0);
  p.setPoint( 6,  5,  0);
  p.setPoint( 7,  2,  0);
  p.setPoint( 8,  0, -2);
  p.setPoint( 9,  0, -5);
  p.setPoint(10,  0, -2);
  p.setPoint(11, -2,  0);
  setPoints(p);
}

FigurePoint::~FigurePoint() {}

/** Re-implemented from QCanvasPolygonalItem. Draws the point */
void FigurePoint::drawShape ( QPainter & p )
{
  p.drawPolygon( areaPoints() );
}
