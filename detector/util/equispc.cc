/***********************************************************************
 * equispc -- return equispaced interpolation of sampled data 
 *
 * usage::  equispc <npts> <filename> <npts> <a> <b> <mode:1,2,3,4>
 *
 * mode is: 1:LIN->LIN, 2:LIN->LOG, 3:LOG->LOG, 4:LOG->LIN
 *
 * the resulting output is an equispaced interpolation
 * of the sampled data in the output mode required
 * between the values x=a and x=b.
 * The result is printed in the stdout.
 *
 * Copyright (C) 1997-2001 J C Gonzalez
 ***********************************************************************/

#include <iostream>
#include <fstream>

#include <cstdio>
#include <cstdlib>
#include <cmath>

int main(int argc, char **argv)
{
  // number of input data points
  int n; 

  // number of output data points
  int npts; 

  // modes
  const char *const modes[]={"LIN->LIN","LIN->LOG","LOG->LOG","LOG->LIN"};

  // counters
  int i, j, k;
  int m;
  int mode;

  // interval width, and limits
  double h;
  double xa, xb;
  double pr;
  
  if (argc<7) {
    cerr << "usage::  equispc <npts> <filename> <npts> <a> <b> <mode:1,2,3,4>" 
         << endl << endl
         << "mode is: 1:LIN->LIN, 2:LIN->LOG, 3:LOG->LOG, 4:LOG->LIN" << endl
         << endl 
         << "the resulting output is an equispaced interpolation" << endl
         << "of the sampled data in the output mode required" << endl
         << "between the values x=a and x=b." << endl
         << "The result is printed in the stdout." << endl;
    return 0;
  }

  // read number of data points
  n = atoi(argv[1]);
  
  // get mode
  // 1: lin, lin
  // 2: lin, log
  // 3: log, log
  // 4: log, lin
  mode = atoi(argv[6]);

  // allocate memory for the input data
  // n is here the TOTAL NUMBER of data points
  double x[n];
  double y[n];
  double a[n];

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

  // allocate memory for the output data
  npts = atoi(argv[3]);
  double newx[npts];
  double newy[npts];

  // get interpolation interval
  xa = atof(argv[4]);
  xb = atof(argv[5]);
  
  h = (xb - xa) / (npts - 1);

  // generate table with coefficients
  for (i=0; i<n; i++) 
    a[i] = y[i];

  for (j=1; j<n; j++) {
    for (i=j; i<n; i++) {
      a[i] = (a[i] - y[i-1]) / (x[i] - x[i-j]);
    }
    for (i=j; i<n; i++) {
      y[i] = a[i];
    }
  }
  
  for (i=0; i<n; i++) 
    cerr << a[i] << endl;

  // show mode of operation
  cerr << "Operating in mode " << modes[mode-1] << endl;
  
  // generate new abscissas
  switch (mode) {
  case 1: // LIN->LIN
  case 3: // LOG->LOG
    for (i=0; i<npts; i++)
      newx[i] = xa + i*h;
    break;
  case 2: // LIN->LOG
    for (i=0; i<npts; i++)
      newx[i] = pow(10.0, xa + i*h);
    break;
  case 4: // LOG->LIN
    for (i=0; i<npts; i++)
      newx[i] = log10(xa + i*h);
    break;
  }

  // calculate new ordinates
  for (j=0; j<npts; j++) {
    newy[j] = a[n-1];
    for (k=n-2; k>=0; k--) {
      newy[j] = a[k] + (newx[j] - x[k]) * newy[j];
    }
  }

  for (i=0; i<npts; i++) 
    cout << xa + i*h << ' ' << newy[i] << endl;
  
  return 0;
}  

  
      
