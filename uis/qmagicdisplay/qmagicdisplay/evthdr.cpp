/*************************************************************************
*                                                                          
* evthdr.cpp  -  description
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


#include "evthdr.h"

#include <iostream.h>

EvtHdr::EvtHdr() {
  nVariables = EvtHdr_NUMBER_OF_VARIABLES;
  data = new float[ nVariables ];
}

EvtHdr::~EvtHdr()
{
  delete [] data;
}

const float & EvtHdr::operator[](int i) const
{
  return *(data+i);
}

float & EvtHdr::operator[](int i)
{
  return *(data+i);
}

const float & EvtHdr::get(int i) const
{
  return *(data+i);
}

float & EvtHdr::get(int i)
{
  return *(data+i);
}

int EvtHdr::size()
{
  return nVariables;
}

void EvtHdr::print()
{
  int i;
  cerr << "Event:" << endl << "----------" << endl;
  for (i=EvtHdr_ntrigger; i<EvtHdr_NUMBER_OF_VARIABLES; i++) {
    cerr << cEvtHdr_Variables[i] << " = " << *(data+i) << endl;
  }
  cerr << endl << flush;
}
// Local Variables:
// mode: c++
// End:
//EOF
