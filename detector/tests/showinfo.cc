#include <iostream>
#include <fstream> 

#include <stdio.h> 
#include <stdlib.h> 

int main(int argc, char **argv)
{
  int i, j, k, l, m;
  float x;

  int inpix;
  inpix = atoi( argv[1] );

  float *fpx = new float[ inpix ];
  float *fpy = new float[ inpix ];

  ifstream ifpixels;
  ifpixels.open("pixels.dat");

  for (i=0; i<inpix; i++) 
    ifpixels >> j >> k >> l >> fpx[i] >> fpy[i];

  ifpixels.close();

  cin >> m;
  j=0;

  while ( m < 0 ) {

    j++;
    
    for (i=0; i<inpix; i++) {

      cin >> x;

      cout << j << ' '
           << x << ' '
           << fpx[i] << ' '
           << fpy[i] << endl;
      
    }
    
    m = 0;

    cin >> m;

  }

  cerr << " m = " << m << endl << flush;
    
  return 0;
}

// Local Variables:
// mode:c++
// mode:font-lock
// End:
