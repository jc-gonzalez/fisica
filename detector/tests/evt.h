#ifndef __EVT_H__
#define __EVT_H__

#include "mcevh.h"

int pre_read_file( char *fname );
void file_locate( float value );
void file_get_data( float *nphe );
MCEventHeader *get_mcevth(void);
void set_read_all_phe(int flag);

#endif
