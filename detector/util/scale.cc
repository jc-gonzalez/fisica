/***********************************************************************
 * scale -- scale log/pow x or y values
 *
 * usage::  scale <modex> <modey> 
 *
 * modex/y is: 0:DO NOTHING   1:TAKE LOG10   2:TAKE POW10
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
  // counters
  int modex, modey;

  // interval width, and limits
  double x, y, newx, newy;

  if (argc < 2) {
    cout << "scale -- scale log/pow x or y values" << endl << endl
         << "usage::  scale <modex> <modey>" << endl << endl
         << "modex/y is: 0:DO NOTHING   1:TAKE LOG10   2:TAKE POW10" << endl;
    exit(0);
  }
  
  modex = atoi(argv[1]);
  modey = atoi(argv[2]);

  do {

    // read data
    cin >> x >> y;

    if (cin.eof()) break;

    // change scales
    if (modex == 1) x = log10(x);
    if (modex == 2) x = pow(10.0, x);

    if (modey == 1) y = log10(y);
    if (modey == 2) y = pow(10.0, y);

    // print it
    cout << x << ' ' << y << endl;
    
  } while ( true );
  
  return 0;
}  

  
      
