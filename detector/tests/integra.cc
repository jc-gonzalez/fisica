#include <iostream>
#include <fstream>

#include <cstdlib>
#include <cmath>

//
// We assume here that the points x0, x1, x2... are xk=x0+kh
//

int main(int argc, char **argv)
{
  // number of data points
  int n; 

  const char *const modes[]={"LIN-LIN","LIN-LOG","LOG-LOG","LOG-LIN"};

  // counters
  int i, j, k;
  int m;
  int mode;

  // interval width
  double h;

  // final value of the integral
  double sum;

  if (argc<3) {
    cerr << "usage::  integra <npts> <filename> <mode:1,2,3,4>" << endl 
         << endl
         << "mode is: 1:LIN-LIN, 2:LIN-LOG, 3:LOG-LOG, 4:LOG-LIN" << endl
         << endl 
         << "the resulting output is F(x)=Int[{t,x,Inf},f[t]]," << endl
         << "and is printed in the same mode." << endl;
    return 0;
  }

  // read number of data points
  n = atoi(argv[1]);
  
  // get mode
  // 1: lin, lin
  // 2: lin, log
  // 3: log, log
  // 4: log, lin
  mode = atoi(argv[3]);

  // allocate memory
  // n is here the TOTAL NUMBER of data points
  double x[n];
  double y[n];

  if (argv[2][0] != '-') {

    ifstream inputfile;
    inputfile.open(argv[2]);
    
    // read data
    for (i=0; i<n; i++)
      inputfile >> x[i] >> y[i];
    
    inputfile.close();

  } else {

    // read data
    for (i=0; i<n; i++)
      cin >> x[i] >> y[i];
    
  }

  cerr << "Operating in mode " << modes[mode-1] << endl;

  switch (mode) {
  case 1:
    break;

  case 2:
    for (i=0; i<n; i++)
      y[i] = pow(10., y[i]);
    break;

  case 3:
    for (i=0; i<n; i++)
      y[i] = pow(10., y[i]) * pow(10., x[i]) * log(10.0);
    break;

  case 4:
    for (i=0; i<n; i++)
      y[i] = y[i] * pow(10., x[i]) * log(10.0);
    break;

  }

  // calculate interval width
  if ( fabs((x[1]-x[0])-(x[n-1]-x[n-2])) > 1.e-4 ) {
    cerr << "Interval width is not uniform!" << endl;
    return 1;
  }
  
  h = x[1] - x[0];

  // if n.points is even -> Simpson + Simpson 3/8 (for four last points)
  // if n.points is odd  -> Simpson

  for (i=0, m=n; m>2; i++, m--) {

    sum = 0.;

    if ( m%2 == 0 ) {
      
      for ( j=0; j<(m-4); j+=2 )
        sum += y[i+j] + 4. * y[i+j+1] + y[i+j+2];

      sum = ((h/3.) * sum + (3.*h/8.) * (y[i+j] +
                                         3. * y[i+j+1] + 3. * y[i+j+2] + 
                                         y[i+j+3]));
      
    } else {
      
      for (j=0; j<m-1; j+=2) 
        sum += y[i+j] + 4. * y[i+j+1] + y[i+j+2];
      
      sum *= (h/3.);
      
    }

    cout << x[i] << ' ';
    if ((mode==2) || (mode==3))
      cout << log10(sum) << endl;
    else 
      cout << sum << endl;

  }

  return 0;
}  

  
      
