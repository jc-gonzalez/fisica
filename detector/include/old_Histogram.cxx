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

#include <iostream.h>
#include <iomanip.h>
#include <fstream.h> 
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "Histogram.hxx"
#include "jcmacros.h"

Histogram::Histogram (Char_t *hname, Int_t nbin, Float_t xmi, Float_t xma)
{
  int i;

  id = nhistos;
  strcpy(name, hname);
  xmin = xmi;
  xmax = xma;
  nbins = nbin;
  ix = (xmax-xmin)/((Float_t)nbins);
  ix_2 = 0.5*ix;
  ntot = 0;
  h = new Float_t[nbins];
  h2 = new Float_t[nbins];
  s = new Float_t[nbins];
  e = new Float_t[nbins];
  n = new int[nbins];
  flags = 0;
  
  for ( i=0; i<nbin; i++) {
    h[i] = h2[i] = s[i] = 0.0;
    n[i] = 0;
  }
  
  nhistos++;
}

Histogram::~Histogram ()
{
  delete [] h;
  delete [] h2;
  delete [] s;
  delete [] e;
  delete [] n;
  h = h2 = s = NULL;
  n = NULL;
}

void
Histogram::fill ( Float_t x, Float_t weight ) 
{
  int i;

  if ( (x >= xmin) && (x <= xmax) ) {
	i = (int)floor(( x - xmin ) / ix);
	h[i] += weight;
	h2[i] += weight*weight;
	++n[i];
	++ntot;
  }
}

void
Histogram::fill ( int i, Float_t weight ) 
{
  --i;
  if ( (i >= 0) && (i < nbins) ) {
	h[i] += weight;
	h2[i] += weight*weight;
	++n[i];
	++ntot;
  }
}

void  
Histogram::setname ( Char_t *newname )
{ 
  strcpy(name, newname); 
}

void  
Histogram::clear ( void ) 
{
  int i;
  for (i=0;i<nbins;i++) {
	h[i] = h2[i] = s[i] = 0.0;
	n[i] = 0;
  }
  flags = ntot = 0;
}
  
void 
Histogram::calcerr ( void )
{
  int i;

  for (i=0; i<nbins; i++) {
    s[i] = sqrt( h[i] ); 
    e[i] = sqrt( h[i] ); 
  }
}

void 
Histogram::calcprof ( void )
{
  int i;

  for (i=0; i<nbins; i++) 
    if ( n[i] > 0 ) {
      h[i] /= n[i]; 
      h2[i] /= n[i]; 
      s[i] = (Float_t)sqrt(((Float_t)n[i]/(Float_t)(n[i] - 1)) * 
			   (h2[i] - SQR(h[i]))); 
      e[i] = s[i] / sqrt(n[i]);
    }
}

void 
Histogram::save ( char *file )
{
  int i;
  Float_t x;

  ofstream fout( file );

  for (i=0, x=xmin; i<nbins; i++, x+=ix)
    fout << x << ' ' << h[i] << ' ' 
	 << ix_2 << ' ' << s[i] << ' ' << e[i] << endl;
  
  fout.close();
}

void 
Histogram::saveoc ( char *file )
{
  int i;
  Float_t x;

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

void 
Histogram::print ( void )
{
  int i, k;
  Float_t max=-1.0e20, min=1.0e20, x;

  for (i=0; i<nbins; i++) {
    max = (h[i] > max) ? h[i] : max;
    min = (h[i] < min) ? h[i] : min;
  }
  
  cout << "----------------------------------------" << endl;
  cout << "HISTO::" << name << "     entries=" << ntot 
	   << "       [" << min << "," << max << "]" << endl;

  if ( ( min == 0.0 ) && ( max == 0.0 ) ){
	cout << "No data" << endl;
	return;
  }

  cout << setiosflags(ios::scientific) << setprecision(3) << setw(10)
	   << "                    "
	   << min
	   << "                                        "
	   << max << endl;
  
  for (i=0, x=xmin; i<nbins; i++, x+=ix ) {

	cout << x << '-' << x+ix << " | ";

    for (k=0; k<((int)(40*(h[i]-min)/(max-min))); k++) 
	  cout << "]";

	cout << endl;

  }

  cout << resetiosflags(ios::scientific);
}

void 
Histogram::dump ( void )
{
  int i;

  cout << "DUMP:: Histogram #" << id << endl
	   << "----------------------------------------" << endl
	   << "name = " << name << endl
	   << "xmin = " << xmin << endl
	   << "xmax = " << xmax << endl
	   << "ix   = " << ix   << endl
	   << "ix_2 = " << ix_2 << endl
	   << "nbins= " << nbins << endl
	   << "ntot = " << ntot << endl
	   << "flags= " << hex << flags << dec << endl
	   << "h[]  = " << h << endl
	   << "h2]  = " << h2<< endl
	   << "s[]  = " << s << endl
	   << "n[]  = " << n << endl << endl;
  
  cout << "-----------+----------+----------+----------" << endl;
  cout << setiosflags(ios::scientific) << setprecision(3) ;

  for (i=0;i<nbins;++i) {
    cout << setw(11) << h[i] 
	 << setw(11) << h2[i] 
	 << setw(11) << s[i] 
	 << setw(11) << e[i] 
	 << setw(11) << n[i] << "\n";
  }
  cout << resetiosflags(ios::scientific);
  
}
