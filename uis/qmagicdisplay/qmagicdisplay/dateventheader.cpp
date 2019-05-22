/*************************************************************************
*                                                                          
* dateventheader.cpp  -  description
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


#include "dateventheader.h"

#include <iostream.h>

DATEventHeader::DATEventHeader() : EventHeader::EventHeader()
{
  _tsource = 0;
}

void DATEventHeader::readEventHeader()
{
  int i;
  QString number;
  for (i=0; i<_evth->size(); i++) {
    (*_tsource) >> number;
    (*_evth)[i] = number.toFloat();
  }
}

void DATEventHeader::printEventHeader()
{
  cerr << _evth->get(1) << ' '
       << _evth->get(2) << ' '
       << _evth->get(3) << ' '
       << _evth->get(_evth->size()-1) << endl;
}

void DATEventHeader::setSource(QDataStream* s)
{
  //debug("DATEventHeader::setSource sets s");
  _source = s;
  CHECK_PTR( _source );

  //debug("DATEventHeader::setSource goes to DATEventHeader::setTextSource");
  setTextSource(new QTextStream(s->device()));
}

void DATEventHeader::setTextSource(QTextStream* s)
{
  if (_tsource != 0) {
    delete _tsource;
    //debug("DATEventHeader::setTextSource delete _tsource");
  }
  //debug("DATEventHeader::setTextSource sets _tsource");
  _tsource = s;
  CHECK_PTR( _tsource );
}

void DATEventHeader::print()
{
  _evth->print();
}

const float & DATEventHeader::operator[](int i) const
{
  return _evth->get(i);
}

const float & DATEventHeader::get(int i) const
{
  return _evth->get(i);
}

// Local Variables:
// mode: c++
// End:
//EOF
