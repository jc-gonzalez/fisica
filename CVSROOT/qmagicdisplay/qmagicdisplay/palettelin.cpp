/*************************************************************************
 *                                                                          
 * palettelin.cpp  -  description
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


#include "palettelin.h"

PaletteLin::PaletteLin()
{
}

PaletteLin::~PaletteLin()
{
}

PaletteMode* PaletteLin::instance = 0;

PaletteMode* PaletteLin::Instance()
{
  if (instance == 0) {
    instance = new PaletteLin;
  }
  return instance;
}

inline PaletteMode::PaletteIndex PaletteLin::getPaletteColor(Charge c, PixSize s)
{
  int cl = int( scaling * (c/(s*s) - minim) - 0.5 );
  if (cl < 0) cl = 0;
  if (cl >= ncolors) cl = ncolors-1;
  return cl;
}

void PaletteLin::initPalette(double a, double b, double ad, double bd, int n)
{
  minim = a;
  maxim = b;
  mindata = ad;
  maxdata = bd;
  ncolors = n;

  scaling = ncolors / (maxim - minim);
}
