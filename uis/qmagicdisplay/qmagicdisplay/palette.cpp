/*************************************************************************
 *                                                                          
 * palette.cpp  -  description
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


#include "palette.h"
#include "palettemode.h"
#include "palettelin.h"
#include "palettelog.h"
#include "paletteauto.h"

#include <qcolor.h>
#include <qtextstream.h>
#include <qfileinfo.h>
#include <qarray.h>
#include <qstrlist.h>

Palette::Palette() :
  Number_of_Colors(64),
  maxim(10), minim(0), maxdata(100), mindata(0)
{
  thePalette = new QArray<QColor>( Number_of_Colors );
}

Palette::~Palette()
{
}

int Palette::get_maxim()
{
  return maxim;
}

int Palette::get_minim()
{
  return minim;
}

double Palette::get_maxdata()
{
  return maxdata;
}

double Palette::get_mindata()
{
  return mindata;
}

void Palette::set_maxim(int n)
{
  maxim = n;
  initPalette();
}

void Palette::set_minim(int n)
{
  minim = n;
  initPalette();
}

void Palette::set_maxdata(double x)
{
  maxdata = x;
  initPalette();
}

void Palette::set_mindata(double x)
{
  mindata = x;
  initPalette();
}

void Palette::setPaletteMode(PaletteScale m)
{
  scale = m;

  switch (scale) {
  	
 	case PALETTE_LINEAR_SCALE:
 	  changeState( PaletteLin::Instance() );
 	  break;
 	
 	case PALETTE_LOGARITHMIC_SCALE:
 	  changeState( PaletteLog::Instance() );
 	  break;
 	
 	case PALETTE_AUTO_SCALE:
 	  changeState( PaletteAuto::Instance() );
 	  break;
 	
  default:
    break;

  }
}

Palette::PaletteScale Palette::getPaletteMode()
{
  return scale;
}
				
Palette::PaletteColor Palette::getPaletteColor(Charge c, PixSize s)
{
  return thePalette->at(state->getPaletteColor(c, s));
}

void Palette::changeState(PaletteMode* pm)
{
  state = pm;
  initPalette();
}

void Palette::initPalette()
{
  state->initPalette(minim, maxim, mindata, maxdata, Number_of_Colors);
}

void Palette::initPalette(double a, double b, double ad, double bd, int n)
{
  state->initPalette(a, b, ad, bd, n);
}

void Palette::load(const char* fname)
{
  QString fileName( fname );
  QFileInfo fpalInfo( fileName );
  QString basename( fpalInfo.baseName() );
  QString absname( fpalInfo.absFilePath() );

  int r, g, b;

  QFile fpal( fileName );
  if ( ! fpal.exists() ) {
    return;
  }

  /** We expect a 1 col. x 64 rows PPM file, formatted as 1 row per line,
   * and therefore we read the whole file and keep the last 64 lines
   */

  qDebug("Reading palette %s ...", fname);
    
  QStrList lines; // we reserv space for 100 lines

  fpal.open( IO_ReadOnly );
  QTextStream t(&fpal);

  while (! t.eof())
    lines.append(t.readLine());

  fpal.close();

  int i, j;
  for (i = (lines.count() - Number_of_Colors), j = 0;
       j < Number_of_Colors;
       i++, j++) {
    sscanf( lines.at(i), "%d %d %d", &r, &g, &b);
    QColor& color = thePalette->at(j);
    color.setRgb( r, g, b);

  }

}
