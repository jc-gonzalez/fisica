/*************************************************************************
*                                                                          
* dateventdata.cpp  -  description
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


#include "dateventdata.h"
#include "magicpar.h"

#include <iostream>

DATEventData::DATEventData() : EventData::EventData()
{
  _tsource = 0;
  _data = new double[ (MAGICPar::Instance())->numberOfPixels ];
}

void DATEventData::readEventData()
{
  int i;
  double dummy;
  (*_tsource) >> dummy;

  QString number;
  (*_tsource) >> number;
  _data[0] = number.toDouble();
  _mindata = _maxdata = _data[0];

  for (i=1; i<(MAGICPar::Instance())->numberOfPixels; i++) {
    (*_tsource) >> number;
    _data[i] = number.toDouble();
    if (_data[i] < _mindata)
      _mindata = _data[i];
    if (_data[i] > _maxdata)
      _maxdata = _data[i];
  }
}

void DATEventData::printEventData()
{
  int i;

  cerr << "DATA: ";
  for (i=0; i<595; i++)
    cerr << _data[i] << ' ';
  cerr << endl << flush;
}

double DATEventData::get_mindata()
{
  return _mindata;
}

double DATEventData::get_maxdata()
{
  return _maxdata;
}

void DATEventData::setSource(QDataStream* s)
{
  _source = s;
  CHECK_PTR( _source );
  setTextSource(new QTextStream(s->device()));
}

void DATEventData::setTextSource(QTextStream* s)
{
  if (_tsource != 0) {
    delete _tsource;
  }
  _tsource = s;
  CHECK_PTR( _tsource );
}

// Local Variables:
// mode: c++
// End:
//EOF
