/*************************************************************************
*                                                                          
* ellipsoid.cpp  -  description
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


#include "ellipsoid.h"

#include <stdio.h>
#include <math.h>

Ellipsoid::Ellipsoid(QCanvas *c) :
  show_ellipse(false),
  show_axes(false),
  show_centroid(false),
  show_maximum(false)
{
  // create elements
  // the size 1000. tries to get enough drawing resolution
  ellipse0      = new QCanvasEllipse(1000, 1000, c);
  ellipse       = new QCanvasPolygon(c);
  axisMain      = new QCanvasLine(c);
  axisSecondary = new QCanvasLine(c);
  centroid      = 0;
  maximum       = 0;

  // check pointer
  CHECK_PTR(ellipse0);
  CHECK_PTR(ellipse);
  CHECK_PTR(axisMain);
  CHECK_PTR(axisSecondary);

  // set attributes
  ellipse->setBrush(QBrush(QColor( Qt::cyan ), Qt::Dense5Pattern));
  ellipse->setX(0);
  ellipse->setY(0);
  ellipse->setZ(1000);

  ellipse0->hide();

  axisMain->setPen( Qt::yellow );
  axisMain->setZ(1001);

  axisSecondary->setPen( Qt::yellow );
  axisSecondary->setZ(1001);

  // create transformations and auxiliary data members
  applyScale_and_Alpha= new QWMatrix;
  totalTransform= new QWMatrix;

  CHECK_PTR(applyScale_and_Alpha);
  CHECK_PTR(totalTransform);

  inverseWorldTransform = 0;

  pts = new QPointArray;
}

Ellipsoid::~Ellipsoid()
{
}

void Ellipsoid::toggleEllipse()
{
  show_ellipse = ! show_ellipse;
}

void Ellipsoid::toggleAxes()
{
  show_axes = ! show_axes;
}

void Ellipsoid::toggleCentroid()
{
  show_centroid = ! show_centroid;
}

void Ellipsoid::toggleMaximum()
{
  show_maximum = ! show_maximum;
}


void Ellipsoid::setZoom(double z)
{
  zoom = z;
  zoom_1000 = z / 1000.;
  zoom_deg = z / 0.1;
  zoom_half = z / 2.;
  postzoom_deg = 1000. / 0.1;
}

void Ellipsoid::setCentroid(double x, double y)
{
  cx = x;
  cy = y;
}

void Ellipsoid::setMaximum(double x, double y)
{
  mx = x;
  my = y;
}

void Ellipsoid::setShape(double l, double w)
{
  length = 2. * l;
  width = 2. * w;
}

void Ellipsoid::setAlpha(double a)
{
  alpha = a;
}

#include <iostream.h>

void Ellipsoid::draw()
{
  if (length < 0.) {
    ellipse->hide();
    axisMain->hide();
    axisSecondary->hide();
    centroid->hide();
    maximum->hide();
    return;
  }

  if (show_ellipse) {
    QPointArray& points = *pts;

    QWMatrix& scale_and_alpha = *applyScale_and_Alpha;
    QWMatrix& transform = *totalTransform;
    QWMatrix& inverse_world_transform = *inverseWorldTransform;

    ellipse0->setSize(length * postzoom_deg, width * postzoom_deg);

    scale_and_alpha.reset();
    scale_and_alpha.rotate(atan2(cy,cx)*180./M_PI + alpha);
    scale_and_alpha.scale(zoom_1000, zoom_1000);

    transform = scale_and_alpha * inverse_world_transform;

    points = transform.map( ellipse0->areaPoints() );
    points.resize(points.count() - 1);

    points.translate( int(cx * zoom_half),
                      int(cy * zoom_half) );

    ellipse->setPoints( points );
    ellipse->show();
  } else {
    ellipse->hide();
  }
  
  if (show_axes) {
    axisMain->show();
    axisSecondary->show();
  } else {
    axisMain->hide();
    axisSecondary->hide();
  }

  if (show_centroid) {
    // we use one of the points of the point array, as auxiliary var.
    pts->at(0) = inverseWorldTransform->map(QPoint(int(cx * zoom_deg),
                                                   int(cy * zoom_deg)));
    centroid->move(pts->at(0).x() - halfside,
                   pts->at(0).y() - halfside);
    centroid->show();
  } else {
    centroid->hide();
  }

  if (show_maximum) {
    // we use one of the points of the point array, as auxiliary var.
    pts->at(0) = inverseWorldTransform->map(QPoint(int(mx * zoom_deg),
                                                   int(my * zoom_deg)));
    maximum->move(pts->at(0).x() - halfside,
                  pts->at(0).y() - halfside);
    maximum->show();
  } else {
    maximum->hide();
  }
}

void Ellipsoid::setWorldTransform(QWMatrix *m)
{
  if ( inverseWorldTransform == 0 )
    inverseWorldTransform = new QWMatrix(*m);

  centroid = new QCanvasRectangle(inverseWorldTransform->map(QRect(-80, -80, 160, 160)),
                                  ellipse->canvas());
  maximum  = new QCanvasRectangle( *centroid );

  halfside = centroid->width() / 2.;

  CHECK_PTR(centroid);
  CHECK_PTR(maximum);

  centroid->setBrush(QBrush(QColor( Qt::magenta ), Qt::SolidPattern));
  centroid->setZ(1002);

  maximum->setBrush(QBrush(QColor( Qt::red ), Qt::SolidPattern));
  maximum->setZ(1002);
}



// Local Variables:
// mode: c++
// End:
//EOF
