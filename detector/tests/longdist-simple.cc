#include <iostream.h>

#include <stdio.h>
#include <stdlib.h>

#define __DEBUG__
#undef  __DEBUG__

int
main(int argc, char **argv)
{
  char filename[256];
  char sdummy[5000];
  int npoints;
  int nshowers;

  double ** depth;
  double ** ldist;
  double ** ildist;

  double * final_depth;
  double * final_ldist;
  double * final_eldist;
  double * final_ildist;
  double * final_eildist;

  double dummy;
  
  int i, j, k;

  nshowers = atoi(argv[1]);
  npoints  = atoi(argv[2]);

  depth = new double * [nshowers];
  for (i=0; i<nshowers; i++) 
    depth[i] = new double [npoints];
  
  ldist = new double * [nshowers];
  for (i=0; i<nshowers; i++) 
    ldist[i] = new double [npoints];

  ildist = new double * [nshowers];
  for (i=0; i<nshowers; i++) 
    ildist[i] = new double [npoints];

  final_depth   = new double [npoints];
  final_ldist   = new double [npoints];
  final_eldist  = new double [npoints];
  final_ildist  = new double [npoints];
  final_eildist = new double [npoints];

  for (i=0; i<nshowers; i++) {
    
    for (j=0; j<npoints; j++) {
      
      cin >> depth[i][j];
      cin >> ldist[i][j];

      if (j>0)
        ildist[i][j] = ldist[i][j] - ldist[i][j-1];
      
#ifdef __DEBUG__
      cout << depth[i][j] << ' '
           << ldist[i][j] << endl;
#endif // __DEBUG__
           
    }

  }

  double sy, sy2, siy, siy2;
  double y, ey;

  for (j=0; j<npoints; j++) {

    sy = sy2 = siy = siy2 = 0.;
    
    for (i=0; i<nshowers; i++) {

      y  =  ldist[i][j];

      sy  += y;
      sy2 += (y*y);

      y  =  ildist[i][j];

      siy  += y;
      siy2 += (y*y);

    }
    
    sy   /= nshowers;
    sy2  /= nshowers;

    siy  /= nshowers;
    siy2 /= nshowers;

    final_depth[j]  = depth[0][j];
    final_ldist[j]  = sy;
    final_eldist[j] = sqrt( sy2 - (sy*sy) ) / sqrt( nshowers );
    final_ildist[j]  = siy;
    final_eildist[j] = sqrt( siy2 - (siy*siy) ) / sqrt( nshowers );

    cout << final_depth[j]   << ' '
         << final_ldist[j]   << ' '
         << final_eldist[j]  << ' '
         << final_ildist[j]  << ' '
         << final_eildist[j] << endl;

  }

  return 0;
}
