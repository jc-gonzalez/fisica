/*************************************************************************
 *                                                                          
 * paletteauto.cpp  -  description
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


#include "paletteauto.h"

PaletteAuto::PaletteAuto()
{
}

PaletteAuto::~PaletteAuto()
{
}

PaletteMode* PaletteAuto::instance = 0;

PaletteMode* PaletteAuto::Instance()
{
  if (instance == 0) {
    instance = new PaletteAuto;
  }
  return instance;
}

inline PaletteMode::PaletteIndex PaletteAuto::getPaletteColor(Charge c, PixSize s)
{
  int cl = int( scaling * (c/(s*s) - mindata) - 0.5 );
  if (cl < 0) cl = 0;
  if (cl >= ncolors) cl = ncolors-1;
  return cl;
}

void PaletteAuto::initPalette(double a, double b, double ad, double bd, int n)
{
  minim = a;
  maxim = b;
  mindata = ad;
  maxdata = bd;
  ncolors = n;

  scaling = ncolors / (maxdata - mindata);
}
