#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>

#include "jcmacros.h"

#include "MyMatrix.hxx"

int 
main(void)
{
  int d[]={2,3,4};
  int k=0, i;

  MyMatrix<int> m(3, d);
  int *f; 

  m.dump();

  while ((f = m.foreach()) != NULL) {
    ++k;
    *f = SQR(k);
  }

  cout << flush;
  cerr << flush;

  m.dump();

  m.idx_reset();

  for (i=0; i<k; ++i) {
    cout << m[(m.idx())] << ' ' 
         << m.idx()[0] << ',' 
         << m.idx()[1] << ',' 
         << m.idx()[2] << ' '
         << endl << flush;
    m.idx_incr();
  }

  return(0);
}
