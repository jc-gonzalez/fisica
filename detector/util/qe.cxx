#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

#include "jcmacros.h"
#include "jcdebug.h"

extern "C" 
{
#include "ranlib.h"
}

void main(int argc, char **argv)
{
  ifstream fin;
  
  float q;

  fin.open(argv[1]);
  int npix = atoi(argv[2]);
  float sigma = atof(argv[3]);

  cout 
	<< "############################################################" << endl
	<< "# Quantum Efficiency file, for each pixel in the camera" << endl
	<< "# Copyright (c) 1998, J C Gonzalez" << endl
	<< "#############################################################" << endl
	<< "# This file is (still) for test purposes only" << endl
	<< "#" << endl
	<< "# first, number of datapoints, and wavelengths" << endl << flush;

  int ndata;
  fin >> ndata;
  cout << ndata << endl;

  float ** m = new float * [2];
  m[0] = new float[ndata];
  m[1] = new float[ndata];

  int i;
  for (i=0; i<ndata; ++i) {
	fin >> m[0][i] >> m[1][i];
	cout << m[0][i] << endl;
  }

  int p;
  for (p=0; p<npix; ++p) {
	
	cout << "# pixel " << p+1 << endl;

	for (i=0; i<ndata; ++i) {
	  while ((q = gennor( m[1][i], sigma ))<0);
	  cout << p+1 << ' ' << i+1 << ' ' << q << endl;
	}

	cout << flush;

  }

}
