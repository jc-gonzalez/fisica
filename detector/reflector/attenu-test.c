#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define attenu attenu_
void attenu(float *, float *, float *, float *, float *, float *, float *);

int main(void)
{

  float wl, h, th, tr;
  float m1, m2, m3;
  int i, j, k, l;

  for (i=0; i<33; i++) {

    wl = 290. + i * 10.;
    
    for (j=0; j<10; j++) {
      
      h  = 3.e5 + j * 10.e5;
      
      for (k=0; k<91; k++) {

        th = (k * 1.) * M_PI / 180.;
        
        attenu( &wl, &h, &th, &tr, &m1, &m2, &m3 );

        printf("%d %d %d %f %f %f %f %f %f %f\n",
               i, j, k, wl, h/1.e5, k*1., tr, m1, m2, m3);
        
      }
      
    }
    
  }

  return 0;
}
