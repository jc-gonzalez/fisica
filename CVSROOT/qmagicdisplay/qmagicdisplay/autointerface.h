/*************************************************************************
*                                                                          
* autointerface.h  -  description
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

#ifndef _AUTOINTERFACE_H 
#define _AUTOINTERFACE_H

#define DECLARE_INTERFACE(C,T,V)  \
T    get_##V(void); \
void set_##V(T _x);

#define DECLARE_INTERFACE_VIRTUAL(C,T,V)  \
virtual T    get_##V(void); \
virtual void set_##V(T _x);

#define DEFINE_IMPLEMENTATION_INLINE(C,T,V)  \
inline T    C##::get_##V(void) { return V; } \
inline void C##::set_##V(T _x) { V = _x; }

#define DEFINE_IMPLEMENTATION(C,T,V)  \
T    C##::get_##V(void) { return V; } \
void C##::set_##V(T _x) { V = _x; }

#endif


// Local Variables:
// mode: c++
// End:
//EOF
