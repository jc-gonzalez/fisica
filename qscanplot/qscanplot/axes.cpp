/*************************************************************************
 *                                                                          
 * axes.cpp  -  description
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


#include "axes.h"

Axes::Axes() : name("Axes1"), description("<No description available>") {}

Axes::~Axes() {}

QTextStream& operator<<(QTextStream & output, const Axes & aAxes)
{
  output << "#--\n";

  output << "# Axes name\n" << aAxes.name << "\n#\n";

  output << "# Axes description\n" << aAxes.description << "\n#\n";

  output << "# Axes points\n";

  int i;
  AxisPoint p;
  for (i=0; i<3; i++) {
    p = aAxes.p[i];
    output << p.getGX() << ' ' << p.getGY() << ' '
           << p.getX()  << ' ' << p.getY() << '\n';
  }

  return output;
}

QTextStream& operator>>(QTextStream & input, Axes & aAxes)
{
  QString s;

  s = "#";
  while (s.at(0) == '#') s=input.readLine();

  aAxes.name  = s;

  qDebug("%% aAxes.name:          [%s]\n", aAxes.name.latin1());

  s = "#";
  while (s.at(0) == '#') s=input.readLine();

  aAxes.description = s;

  qDebug("%% aAxes.description:   [%s]\n", aAxes.description.latin1());

  s = "#";
  while (s.at(0) == '#') s=input.readLine();

  int i;
  double gx, gy, x, y;
  for (i=0; i<3; i++) {
    input >> gx >> gy >> x >> y;
    aAxes.p[i].setGX( gx );
    aAxes.p[i].setGY( gy );
    aAxes.p[i].setX( x );
    aAxes.p[i].setY( y );
		qDebug("%% aAxes.point %d:  %f %f  ->  %f %f\n", i, gx, gy, x, y);
  }

  return input;
}

