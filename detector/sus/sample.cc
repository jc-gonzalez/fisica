//!/////////////////////////////////////////////////////////
//
// sample
//
// @file        sample.cc
// @title       Sample source file
// @desc        Sample output from a C++ file
// @author      J C Gonzalez
// @email       gonzalez@mppmu.mpg.de
//
////////////////////////////////////////////////////////////
// @tableofcontents @coverpage

//# About this program

/*" This program just writes the first {\tt N} integer numbers,
   starting from 1, and their {\tt k}-power.  The syntax of this
   program is:\par \centerline{\tt sample <N> <k>} */

//# The code

//{

//## Included header files

#include <iostream.hxx>
#include <stdlib.h>

//## Prototypes

int powk(int n, int k);

//## Main program

int
main (int argc, char **argv)
{

  // local variables 

  register int i, j;
  int n, k;

  /*' This is a comment in the middle of the code. It could be have
     written here, for example, to clarify some special and/or tricky
     thing in the core of the code.  */
  
  // check command line
  
  if (argc<3) {
    cerr << "Syntax::  sample <n> <k>\n\n";
    exit(1);
  }

  // get input values

  n = atoi(argv[1]);
  k = atoi(argv[2]);
  
  // list the result

  for ( i=1; i<=n; ++i )
    cout << i << '\t' << powk(i,k) << '\n';

  exit(0);
}
//}
  
//## Functions

//!-----------------------------------------------------------
//@name powk
//
//@desc Returns the power k of an integer number n
//
//@date Wed Jul  8 15:25:39 MET DST 1998
//------------------------------------------------------------
//@function

//{
int 
powk(int n, int k)
{
  register int j;
  int m;

  for ( j=1,m=n; j<k; j++,m*=n );

  return (m);
}
//}
