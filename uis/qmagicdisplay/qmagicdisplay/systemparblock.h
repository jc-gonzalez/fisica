/*************************************************************************
 *                                                                          
 * systemparblock.h  -  description
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


#ifndef SYSTEMPARBLOCK_H
#define SYSTEMPARBLOCK_H

#include <inidb.h>

/**Class for storing the system-specific parameters.
  *Used by SystemPar.
  *@author J C Gonzalez
  */

class SystemParBlock : public IOParamBlock  {
public: 
	SystemParBlock();
	~SystemParBlock();

private:
  DECL_INTERFACE_NAMED_VAL(string, palette_path);
  DECL_INTERFACE_NAMED_VAL(bool, splash);
};

#endif
