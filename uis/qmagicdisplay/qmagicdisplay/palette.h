/*************************************************************************
 *                                                                          
 * palette.h  -  description
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


#ifndef PALETTE_H
#define PALETTE_H

#include "autointerface.h"

#include <qarray.h>

/**
  *@author J C Gonzalez
  */

class QColor;
class PaletteMode;

class Palette {
public: 
	Palette();
	virtual ~Palette();

	typedef QColor& PaletteColor;
	typedef double Charge;
  typedef int    PixSize;
  typedef int    PaletteScale;

	enum {PALETTE_LINEAR_SCALE,
	      PALETTE_LOGARITHMIC_SCALE,
	      PALETTE_AUTO_SCALE};

  const int Number_of_Colors;
	        	
	DECLARE_INTERFACE(Palette,int,maxim);
	DECLARE_INTERFACE(Palette,int,minim);
	DECLARE_INTERFACE(Palette,double,maxdata);
	DECLARE_INTERFACE(Palette,double,mindata);

  void         setPaletteMode(PaletteScale m);
  PaletteScale getPaletteMode();
	PaletteColor getPaletteColor(Charge c, PixSize s);
  void         load(const char* fname);
	
private:
  friend class PaletteMode;
  void changeState(PaletteMode* ps);
  void initPalette();
  void initPalette(double a, double b, double ad, double bd, int n);

private:
  PaletteMode* state;
  PaletteScale scale;
  int maxim;
  int minim;
  double maxdata;
  double mindata;

  QArray<QColor>* thePalette;
};

#endif
