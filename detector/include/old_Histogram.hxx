/////////////////////////////////////////////////////////////////
//
// Histogram
//
//  Created: Thu May  7 16:24:22 1998
//  Author:  Jose Carlos Gonzalez
//  Purpose: Implementation of Histograms
//  Notes:   
//  
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt Histograms.hxx}

/* @text
This section shows the include file {\tt Histograms.hxx}
@endtext */

// @subsection Class {\em Histogram}: Definition

// @code
//////////////////////////////////////////////////////////////////////
// Histogram classes :: definition 
// 
// Small class for histograming data
//--------------------------------------------------------------------
// Copyright (c) 1998, J C Gonzalez
//
// Permission to use, copy, modify, distribute and sell this software
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear
// in supporting documentation. This piece of code is provided "as is" 
// WITHOUT ANY KIND of express or implied warranty.
//////////////////////////////////////////////////////////////////////

#ifndef __HISTOGRAM_HXX__
#define __HISTOGRAM_HXX__

// include files

#include <iostream.h>
#include <iomanip.h>
#include <fstream.h> 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "jcmacros.h"

// number of histograms in memory

int nhistos = 0;

//----------------------------------------
// class Histogram
//----------------------------------------

template<class T>
class Histogram {

private:
  char name[40];            // name of the histogram
  int id;                   // identifier
  T xmin, xmax;             // limits of the histogram
  T ix, ix_2;               // bin size
  T *h, *h2, *s, *e;        // data of the histogram
  int nbins, *n, ntot, ns;  // information about the binning
  int flags;                // flags
  int imx;                  // bin with maximum value

public:
  // constructor
  Histogram(char *hname=" ", int nbin=10,  
            T xmi=0., T xma=1.);
  
  // destructor
  ~Histogram(void);

  // copy constructor defaults

  // get identifier
  inline int getid() { return ( id ); }

  // fills the histogram
  void stack (void);
  void fill( T x, T weight=1.0 );
  void fill( int i, T weight=1.0 );
  T & operator[](int i);
  T & operator[](T x);

  // get sum of weights
  inline int getsum() { return ( ntot ); }

  // get contents of bin i
  inline T getbin ( int i ) { return ( h[i-1] ); }

  // get bin for value x
  inline T getbin ( T x ) { return ( h[x] ); }

  // get imax
  inline int imax( void ) { return( imx ); }

  // get ximax
  inline float ximax( void ) { return( xmin + ix*imx +ix_2 ); }

  // get yimax
  inline float yimax( void ) { return( h[imx] ); }

  // clear histogram
  void clear ( void );

  // change name of histogram
  void setname ( char *newname );

  // calculate errors
  void calcerr ( void );

  // calculate mean in each bin and errors (as in a profile
  void calcprof ( void );

  // save for octave
  void saveoc ( char *file );

  // save in ASCII
  void save ( char *file );

  // print a term-view of the histogram
  void print ( void );

  // dump information
  void dump ( void );

};

// @endcode

// @T \newpage

// @subsection Class {\em Histogram}: Implementation

// @code

template<class T>
Histogram<T>::Histogram (char *hname, int nbin, 
		      T xmi, T xma)
{
  int i;

  id = nhistos;
  strcpy(name, hname);
  xmin = xmi;
  xmax = xma;
  nbins = nbin;
  ix = (xmax-xmin)/((T)nbins);
  ix_2 = 0.5*ix;
  ntot = ns = 0;
  h = new T[nbins];
  h2 = new T[nbins];
  s = new T[nbins];
  e = new T[nbins];
  n = new int[nbins];
  flags = imx = 0;
  
  for ( i=0; i<nbin; i++) {
    h[i] = h2[i] = s[i] = e[i] = 0.0;
    n[i] = 0;
  }
  
  nhistos++;
}

template<class T>
Histogram<T>::~Histogram ()
{
  delete [] h;
  delete [] h2;
  delete [] s;
  delete [] e;
  delete [] n;
  h = h2 = s = NULL;
  n = NULL;
}


template<class T>
void Histogram<T>::stack (void) 
{
  int i;

  for (i=0; i<nbins; ++i) {
	s[i] += h[i];
    e[i] += SQR(h[i]);
	++ns;
  }
}


template<class T>
void Histogram<T>::fill ( T x, T weight ) 
{
  int i;

  if ( (x >= xmin) && (x <= xmax) ) {
	i = (int)floor(( x - xmin ) / ix);
	h[i] += weight;
	h2[i] += weight*weight;
	++n[i];
	++ntot;
    if (h[imx]<h[i]) imx=i;
  }
}


template<class T>
void Histogram<T>::fill ( int i, T weight ) 
{
  --i;
  if ( (i >= 0) && (i < nbins) ) {
	h[i] += weight;
	h2[i] += weight*weight;
	++n[i];
	++ntot;
    if (h[imx]<h[i]) imx=i;
  }
}


template<class T>
T &Histogram<T>::operator[](int i)
{
  --i;
  return( h[ i ] );
}


template<class T>
T &Histogram<T>::operator[](T x)
{
  int i=0;

  if ( (x >= xmin) && (x <= xmax) ) 
	i = (int)floor(( x - xmin ) / ix);
  return( h[ i ] );
}


template<class T>
void Histogram<T>::setname ( char *newname )
{ 
  strcpy(name, newname); 
}


template<class T>
void Histogram<T>::clear ( void ) 
{
  int i;
  for (i=0;i<nbins;i++) {
	h[i] = h2[i] = s[i] = 0.0;
	n[i] = 0;
  }
  flags = ntot = ns = 0;
}
  

template<class T>
void Histogram<T>::calcerr ( void )
{
  int i;

  for (i=0; i<nbins; i++) {
    s[i] = sqrt( h[i] ); 
    e[i] = sqrt( h[i] ); 
  }
}


template<class T>
void Histogram<T>::calcprof ( void )
{
  int i;

  for (i=0; i<nbins; i++) {
    h[i] = s[i]/ns; 
    h2[i] = e[i]/ns;
    n[i] = ns;
    s[i] = (T)sqrt(((T)n[i]/(T)(n[i] - 1)) * 
                   (h2[i] - SQR(h[i]))); 
    e[i] = s[i] / sqrt(n[i]);
  }
}

template<class T>
void Histogram<T>::save ( char *file )
{
  int i;
  T x;

  ofstream fout( file );

  for (i=0, x=xmin; i<nbins; i++, x+=ix)
    fout << x << ' ' << h[i] << ' ' 
	 << ix_2 << ' ' << s[i] << ' ' << e[i] << endl;
  
  fout.close();
}

template<class T>
void Histogram<T>::saveoc ( char *file )
{
  int i;
  T x;

  ofstream fout( file );

  fout << "#name: " << name << endl;
  fout << "#type: matrix" << endl;
  fout << "#rows: " << nbins << endl;
  fout << "#columns: 5" << endl;

  for (i=0, x=xmin; i<nbins; i++, x+=ix)
	fout << x << ' ' << h[i] << ' ' 
	     << ix_2 << ' ' << s[i] << ' ' << e[i] << endl;
  
  fout.close();
}

template<class T>
void Histogram<T>::print ( void )
{
  int i, k;
  T max=-1.0e20, min=1.0e20, x;

  for (i=0; i<nbins; i++) {
    max = (h[i] > max) ? h[i] : max;
    min = (h[i] < min) ? h[i] : min;
  }
  
  printf ("----------------------------------------\n");
  printf ("HISTO::%s    entries=%d        [%f,%f]\n", 
          name, ntot, min, max);

  if ( ( min == 0.0 ) && ( max == 0.0 ) ){
	printf("No data\n");
	return;
  }

  printf ("                    %10.3f", min);
  printf ("                                        %10.3f\n", max);
  
  for (i=0, x=xmin; i<nbins; i++, x+=ix ) {
    printf("%11.2f:%11.2f | ", x, x+ix);
    for (k=0; k<((int)(40*(h[i]-min)/(max-min))); k++) 
	  putchar(']');
    printf("\n");

  }
}

template<class T>
void Histogram<T>::dump ( void )
{
  int i;

  printf("\nDUMP:: Histogram #%d\n", id);
  printf("----------------------------------------\n");
  printf("name = %s\n", name);
  printf("xmin = %f\n", xmin);
  printf("xmax = %f\n", xmax);
  printf("ix   = %f\n", ix);
  printf("ix_2 = %f\n", ix_2);
  printf("nbins= %d\n", nbins);
  printf("ntot = %d\n", ntot);
  printf("----------------------------------------\n");

  for (i=0;i<nbins;++i) {
    printf("%10.3 %10.3 %10.3 %10.3 %10.3\n", 
           h[i], h2[i], s[i], e[i], n[i]);
  }
  
}

#endif 

// @endcode 
//EOF
