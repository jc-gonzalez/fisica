/***********************************************************************
 * genflux -- generate a flux curve
 *
 * Copyright (C) 1997-2001 J C Gonzalez
 ***********************************************************************/

#include <iostream>
#include <fstream>

#include <cstdlib>
#include <cmath>

int main(int argc, char **argv)
{
  // number of data points
  int n; 

  const char *const modes[]={"Int->Int (lin)", "Int->Int (log)",
                             "Dif->Dif (lin)", "Dif->Dif (log)",
                             "Int->Dif (lin)", "Int->Dif (log)",
                             "Dif->Int (lin)", "Dif->Int (log)"};
  // counters
  int i, j, k;
  int m;
  int mode;

  // interval width
  double h;
  double Ei, Ef;

  // calculated values
  double x, y;
  double powx, logy;

  // constants in the flux and dif.flux expressions
  double C;     // = A or B, depending on the mode
  double gamma; // = alpha or beta, depending on the mode

  // help message is not enough arguments
  if (argc<6) {

    cerr << "usage::" << endl
         << "\tcases  Int->*  (modes 1,2,5,6)" << endl
         << endl 
         << "\t  genflux <npts> <A> <alpha> <Ei> <Ef> <mode>" << endl
         << endl 
         << "\tcases  Dif->*  (modes 3,4,7,8)" << endl
         << endl 
         << "\t  genflux <npts> <B> <beta>  <Ei> <Ef> <mode>" << endl 
         << endl
         << "mode is: " << endl;

    // show modes
    for(i=0; i<8; i++)
      cerr << "\t" << i+1 << ":" << modes[i] << endl;

    cerr << endl
         << "The values of Ei and Ef are supposed to be in " << endl
         << "log. scale, and the output is printed in the linear" << endl
         << "or log.scale, according to the selected mode." << endl
         << endl
         << "The result is printed in the standard output." << endl;

    cerr << endl
         << "The reference flux expressions are assumed to be:" << endl
         << endl
         << "                             dF(E)               " << endl
         << " F(>E)= A E^(-alpha)  and    ----- = B E^(-beta) " << endl
         << "                              dE                 " << endl;

    // exit
    return 0;
    
  }

  // read number of data points
  n = atoi(argv[1]);
  
  // get constans (= A or B)
  C = atof(argv[2]);

  // get spectral index (= alpha or beta)
  gamma = atof(argv[3]);

  // get energy limits
  Ei = atof(argv[4]);
  Ef = atof(argv[5]);

  // get mode
  mode = atoi(argv[6]);

  cerr << "Operating in mode " << modes[mode-1] << endl;

  // set log.interval
  h = (Ef - Ei) / (n - 1.0);

  for (i=0, x=Ei; i<n; i++, x+=h) {
    
    x = Ei + i * h;
    powx = pow(10.0, x);
    
    switch (mode) {
      
    case 1: // Int->Int
    case 2: // Int->Int
    case 3: // Dif->Dif
    case 4: // Dif->Dif
      y = C * pow(powx, -gamma);
      break;
      
    case 5: // Int->Dif
    case 6: // Int->Dif
      y = (gamma * C) * pow(powx, -(gamma+1));
      break;

    case 7: // Dif->Int
    case 8: // Dif->Int
      y = (C / (1 - gamma)) * pow(powx, -(gamma-1));;
      break;
      
    }

    logy = log10(y);
    
    switch (mode) {

    case 1: // Int->Int
    case 3: // Dif->Dif
    case 5: // Int->Dif
    case 7: // Dif->Int
      cout << powx << ' ' << y << endl;
      break;
      
    case 2: // Int->Int
    case 4: // Dif->Dif
    case 6: // Int->Dif
    case 8: // Dif->Int
      cout << x << ' ' << logy << endl;
      break;
      
    } // end switch

  } // end for
  
  return 0;
}  

  
      
