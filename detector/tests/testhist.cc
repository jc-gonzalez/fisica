#include "Histogram.h"

#include <iostream>
#include <fstream> 


int main(void)
{
  double x, ww;
  int i;

  Histogram<double> v("Prueba", 140, -8.0, 20.0);

  for (i=0; i<10000; i++) { 
    cin >> x;
    v.fill(x);
  }

  
  ifstream ifpulse;
  ifpulse.open("pulse.dat");

  Histogram<double> w("pulso", 200, 0.0, 10.0);

  for (i=0; i<w.get_nbins(); i++) {
    ifpulse >> x >> ww;
    //cerr << x << ' ' << ww << endl << flush;
    w.fill(x, ww, 0.0);
  }

  ifpulse.close();
  
  Histogram<double> u("Convolution", 140, -8.0, 20.0);

  u.save("convolution.oc", "convolution");
  v.save("original.oc", "original");
  w.save("pulse.oc", "pulse");

  for (int j=0; j<1000; j++) {

    convolute(v, w, u);
    
    for(i=0; i<v.get_nbins(); i++) { 
      cout << v.get_center(i)  << ' '
           << v.get_content(i) << ' '
           << v.get_entries(i) << ' '
           << v.get_error(i)   << '\t'
           << u.get_center(i)  << ' '
           << u.get_content(i) << ' '
           << u.get_entries(i) << ' '
           << u.get_error(i)   << '\n';
    }
    
    for(i=0; i<w.get_nbins(); i++) { 
      cerr << w.get_center(i)  << ' '
           << w.get_content(i) << ' '
           << w.get_entries(i) << ' '
           << w.get_error(i)   << '\n';
    }

    cerr << j << endl << endl << flush;
  
    cerr << "ENTRIES:    " << v.get_entries()  << endl << flush;
    cerr << "SUM:        " << v.get_sum()      << endl << flush;
    cerr << "NBINS:      " << v.get_nbins()    << endl << flush;
    cerr << "BINSIZE:    " << v.get_binsize()  << endl << flush;
    cerr << "MEAN:       " << v.get_mean()     << endl << flush;
    cerr << "VARIANCE:   " << v.get_variance() << endl << flush;
    cerr << "STDEV:      " << v.get_stdev()    << endl << flush;
    cerr << "SIGMA:      " << v.get_sigma()    << endl << flush;
    cerr << "RMS:        " << v.get_rms()      << endl << flush;
    cerr << "MEDIAN:     " << v.get_median()   << endl << flush;
  
    cerr << endl << flush;
  
    cerr << "ENTRIES:    " << u.get_entries()  << endl << flush;
    cerr << "SUM:        " << u.get_sum()      << endl << flush;
    cerr << "NBINS:      " << u.get_nbins()    << endl << flush;
    cerr << "BINSIZE:    " << u.get_binsize()  << endl << flush;
    cerr << "MEAN:       " << u.get_mean()     << endl << flush;
    cerr << "VARIANCE:   " << u.get_variance() << endl << flush;
    cerr << "STDEV:      " << u.get_stdev()    << endl << flush;
    cerr << "SIGMA:      " << u.get_sigma()    << endl << flush;
    cerr << "RMS:        " << u.get_rms()      << endl << flush;
    cerr << "MEDIAN:     " << u.get_median()   << endl << flush;

    cerr << endl << flush;
  
    cerr << "ENTRIES:    " << w.get_entries()  << endl << flush;
    cerr << "SUM:        " << w.get_sum()      << endl << flush;
    cerr << "NBINS:      " << w.get_nbins()    << endl << flush;
    cerr << "BINSIZE:    " << w.get_binsize()  << endl << flush;
    cerr << "MEAN:       " << w.get_mean()     << endl << flush;
    cerr << "VARIANCE:   " << w.get_variance() << endl << flush;
    cerr << "STDEV:      " << w.get_stdev()    << endl << flush;
    cerr << "SIGMA:      " << w.get_sigma()    << endl << flush;
    cerr << "RMS:        " << w.get_rms()      << endl << flush;
    cerr << "MEDIAN:     " << w.get_median()   << endl << flush;
  
  }
  
  u.save("convolution.oc", "convolution");
  v.save("original.oc", "original");
  w.save("pulse.oc", "pulse");

  return 0;
}

// Local Variables:
// mode:c++
// mode:font-lock
// End:
