#include <iostream.h>
#include <iomanip.h>
#include <fstream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "histogram.h"
#include "jcmacros.h"

histogram::histogram (char *hname, Float_t xmi, Float_t xma, int nbin)
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
  n = new int[nbins];
  flags = 0;
  
  for ( i=0; i<nbin; i++) {
    h[i] = h2[i] = s[i] = 0.0;
    n[i] = 0;
  }
  
  nhistos++;
}

histogram::~histogram ()
{
  delete [] h;
  delete [] h2;
  delete [] s;
  delete [] n;
  h = h2 = s = NULL;
  n = NULL;
}

void
histogram::fill ( Float_t x, Float_t weight ) 
{
  int i;

  if ( (x >= xmin) && (x <= xmax) ) {
	i = floor(( x - xmin ) / ix);
	h[i] += weight;
	h2[i] += weight*weight;
	++n[i];
	++ntot;
  }
}

void
histogram::fill ( int i, Float_t weight ) 
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
histogram::clear ( void ) 
{
  int i;
  for (i=0;i<nbins;i++) {
	h[i] = h2[i] = s[i] = 0.0;
	n[i] = 0;
  }
  flags = ntot = 0;
}
  
void 
histogram::calcstat ( void )
{
  int i;

  for (i=0; i<nbins; i++)
    s[i] = sqrt( h[i] ); 
}

void 
histogram::calcavg ( void )
{
  int i;

  for (i=0; i<nbins; i++) {
    h[i] /= n[i]; 
    h2[i] /= n[i]; 
    s[i] = (Float_t)sqrt(((Float_t)n[i]/(Float_t)(n[i] - 1)) * (h2[i] - SQR(h[i]))); 
  }
}

void 
histogram::save ( char *file )
{
  int i;
  Float_t x;

  ofstream fout( file );

  for (i=0, x=xmin; i<nbins; i++, x+=ix)
	fout << x << " " << h[i] << " " << ix_2 << " " << s[i] << endl;
  
  fout.close();
}

void 
histogram::saveoc ( char *file )
{
  int i;
  Float_t x;

  ofstream fout( file );

  fout << "#name: " << name << endl;
  fout << "#type: matrix" << endl;
  fout << "#rows: " << nbins << endl;
  fout << "#columns: 4" << endl;

  for (i=0, x=xmin; i<nbins; i++, x+=ix)
	fout << x << " " << h[i] << " " << ix_2 << " " << s[i] << endl;
  
  fout.close();
}

void 
histogram::print ( void )
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
histogram::dump ( void )
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
		 << setw(11) << n[i] << "\n";
  }
  cout << resetiosflags(ios::scientific);

}
