#ifndef _analysis_
#define _analysis_

#ifndef _this_
#define _this_ analysis
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>

#include "jcmacros.h"

#define PIX_ARRAY_SIDE       40
#define PIX_ARRAY_HALF_SIDE  20
#define PIXNUM               0
#define PIXX                 1
#define PIXY                 2

#define FIRST_PIXEL(r,s)   ( ((r)>0) ? (3*(r)*((r)-1) + (r)*(s) + 1) : 0 )
#define NUMBER_PIXELS(r)   ( ((r)>0) ? FIRST_PIXEL((r)+1,0) : 1 )

void analysis( int n, float *image, int flag);
void get_charge(float *c,float *x);
void get_maxs(float *m, int *nm);
void get_hillas(float *len, float *wid, float *d, float *xd, 
                float *aw, float *mis, float *alph, float *cnc);
void get_asym(float *asx, float *asy, float *p);
void read_pixels(int n);
void set_threshold(float t);
int pixels_are_neig(int pix1, int pix2);

#endif 

