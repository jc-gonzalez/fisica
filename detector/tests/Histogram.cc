//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// template class Histogram :: implementation
//
// Uses STL functors and containers
//
//
// Kopyleft (k) 1999, 2000   J.C.Gonzalez (gonzalez@mppmu.mpg.de)
//----------------------------------------------------------------------
//

#pragma implementation

#include "Histogram.h"

#include <iostream>
#include <fstream>
#include <iomanip>

template<class T>
Histogram<T>::Histogram(string hname, int n, T xmin, T xmax)
{

  nbins   = n;
  maximum = xmax;
  minimum = xmin;

  binsize   = (maximum - minimum) /  static_cast<T>(nbins);
  binsize_2 = binsize / static_cast<T>(2);
    
  histname = hname;
  
  nentries   = new vector<int>(nbins);
  abscissas  = new vector<T>(nbins);
  abscissas2 = new vector<T>(nbins);
  data       = new vector<T>(nbins);
  data2      = new vector<T>(nbins);
  errors     = new vector<T>(nbins);

  vector<T> tmpvec(nbins);
  std::fill(tmpvec.begin(), tmpvec.end(), binsize);
  std::iota(abscissas->begin(), abscissas->end(), zero);
  std::transform(abscissas->begin(), abscissas->end(),
                 tmpvec.begin(), abscissas->begin(), multiplies<T>());
  std::fill(tmpvec.begin(), tmpvec.end(), minimum);
  std::transform(abscissas->begin(), abscissas->end(),
                 tmpvec.begin(), abscissas->begin(), plus<T>());
  std::transform(abscissas->begin(), abscissas->end(),
                 abscissas->begin(), abscissas2->begin(), multiplies<T>());

  clear();
  
}

template<class T>
Histogram<T>::Histogram(Histogram<T> const & x)
{
  nbins       = x.nbins;        
  minimum     = x.minimum;      
  maximum     = x.maximum;      
  binsize     = x.binsize;      
  binsize_2   = x.binsize_2;
  
  nentries    = new vector<int>(*(x.nentries));
  abscissas   = new vector<T>(*(x.abscissas) );
  abscissas2  = new vector<T>(*(x.abscissas2));
  data        = new vector<T>(*(x.data)      );
  data2       = new vector<T>(*(x.data2)     );
  errors      = new vector<T>(*(x.errors)    );
}

template<class T>
Histogram<T>::Histogram()
{
  nbins      = 0;
  maximum    = zero;
  minimum    = zero;
  binsize    = zero;
  binsize_2  = zero;
    
  histname   = "";
  
  nentries   = 0;
  abscissas  = 0; 
  abscissas2 = 0; 
  data       = 0; 
  data2      = 0; 
  errors     = 0; 
}

template<class T>
Histogram<T>::~Histogram()
{
}

template<class T>
const int Histogram<T>::get_nbins(void)
{
  return nbins;
}

template<class T>
const T Histogram<T>::get_binsize(void)
{
  return binsize;
}

template<class T>
const T Histogram<T>::get_upper(void)
{
  return maximum;
}

template<class T>
const T Histogram<T>::get_lower(void)
{
  return minimum;
}

template<class T>
const int Histogram<T>::get_nbin(T& x)
{
  return static_cast<int>(floor( (x - minimum) / binsize ));
}

template<class T>
const T Histogram<T>::get_upper(int i)
{
  return (minimum + (i * binsize)); 
}

template<class T>
const T Histogram<T>::get_lower(int i)
{
  T tmp = (minimum + ((i + 1) * binsize));
  return tmp;
}

template<class T>
const T Histogram<T>::get_center(int i)
{
  T tmp = (minimum + static_cast<T>(i * binsize) + binsize_2);
  return tmp;
}

template<class T>
const T Histogram<T>::get_content(int i)
{
  return (*data)[i];
}

template<class T>
const int Histogram<T>::get_entries(int i)
{
  return (*nentries)[i];
}

template<class T>
const T Histogram<T>::get_error(int i)
{
  return (*errors)[i];
}

template<class T>
const T Histogram<T>::get_sum(void)
{
  sum = std::accumulate(data->begin(), data->end(), zero);
  return sum;
}

template<class T>
const int Histogram<T>::get_entries(void)
{
  entries = std::accumulate(nentries->begin(), nentries->end(), 0);
  return entries;
}

template<class T>
const T Histogram<T>::get_mean(void)
{
  mean = (std::inner_product(data->begin(), data->end(),
                             abscissas->begin(), zero) / get_sum());
  return mean;
}

template<class T>
const T Histogram<T>::get_variance(void)
{
  T tmpmean = get_mean();
  
  variance = (std::inner_product(data->begin(), data->end(),
                                 abscissas2->begin(), zero) /
              get_sum()) - (tmpmean * tmpmean);
  
  return variance;
}

template<class T>
const T Histogram<T>::get_stdev(void)
{
  stdev = sqrt( get_variance() );
  
  return stdev;
}

template<class T>
const T Histogram<T>::get_sigma(void)
{
  return ( get_stdev() );
}

template<class T>
const T Histogram<T>::get_median(void)
{
  T tmpsum = get_sum();
  T half   = tmpsum / static_cast<T>(2);
  T cumsum = zero;
  T xa     = minimum - binsize;
  T xb     = minimum;

  T * first = (*data).begin();
  T * last  = (*data).end();

  while (first != last) {
    xa = xb;
    xb += binsize;
    cumsum += (*first);
    if ( cumsum > half )
      break;
    first++;
  }

  cumsum -= (*first);
  median = xa + (xb - xa) * (half - cumsum) / (*first); 

  return  median;
}

template<class T>
const T Histogram<T>::get_rms(void)
{
  return zero;
}

template<class T>
void Histogram<T>::fill(T& x)
{
  if ( (x > minimum) && (x < maximum) ) {
    int i = Histogram<T>::get_nbin( x );
    (*nentries)[ i ]++;
    (*data)[ i ]   = (*data)[ i ] + one;
    (*data2)[ i ]  = (*data2)[ i ] + one;
    (*errors)[ i ] = sqrt( (*data)[ i ] );
  }
}

template<class T>
void Histogram<T>::fill(T& x, T w)
{
  if ( (x > minimum) && (x < maximum) ) {
    int i = Histogram<T>::get_nbin( x );
    (*nentries)[ i ]++;
    (*data)[ i ]   = (*data)[ i ] + w;
    (*data2)[ i ]  = (*data2)[ i ] + (w * w);
    (*errors)[ i ] = (*errors)[ i ] + one / sqrt( w );
  }
}

template<class T>
void Histogram<T>::fill(T& x, T w, T e)
{
  if ( (x > minimum) && (x < maximum) ) {
    int i = Histogram<T>::get_nbin( x );
    (*nentries)[ i ]++;
    (*data)[ i ]   = (*data)[ i ] + w;
    (*data2)[ i ]  = (*data2)[ i ] + (w * w);
    (*errors)[ i ] = (*errors)[ i ] + e;
  }
}

template<class T>
void Histogram<T>::fill(int i, T w)
{
  if ( (i >= 0) && (i < nbins) ) {
    (*nentries)[ i ]++;
    (*data)[ i ]   = (*data)[ i ] + w;
    (*data2)[ i ]  = (*data2)[ i ] + (w * w);
    (*errors)[ i ] = (*errors)[ i ] + one / sqrt( w );
  }
}

template<class T>
void Histogram<T>::fill(int i, T w, T e)
{
  if ( (i >= 0) && (i < nbins) ) {
    (*nentries)[ i ]++;
    (*data)[ i ]   = (*data)[ i ] + w;
    (*data2)[ i ]  = (*data2)[ i ] + (w * w);
    (*errors)[ i ] = (*errors)[ i ] + e;
  }
}

template<class T>
void Histogram<T>::fillall(T& w)
{
  for (int i=0; i<nbins; i++) {
    (*nentries)[ i ] = 1;
    (*data)[ i ]   = w;
    (*data2)[ i ]  = (w * w);
    (*errors)[ i ] = 1./sqrt( w );
  }
}

template<class T>
void Histogram<T>::scale(T& w)
{
  for (int i=0; i<nbins; i++) {
    (*nentries)[ i ] = 1;
    (*data)[ i ]   *= w;
    (*data2)[ i ]  *= (w * w);
    (*errors)[ i ] *= w;
  }
}

template<class T>
void Histogram<T>::clear(void)
{
  std::fill(data->begin(), data->end(), zero);
  std::fill(data2->begin(), data2->end(), zero);
  std::fill(errors->begin(), errors->end(), zero);
  std::fill(nentries->begin(), nentries->end(), 0);
  
  entries = 0;
  sum     = zero;
  rms     = zero;
  mean    = zero;
  median  = zero;
}

template<class T>
Histogram<T> const & Histogram<T>::operator=(Histogram<T> const & x)
{
  if (this != &x) {

    delete nentries  ;
    delete abscissas ;
    delete abscissas2;
    delete data      ;
    delete data2     ;
    delete errors    ;

    nbins       = x.nbins    ;        
    minimum     = x.minimum  ;      
    maximum     = x.maximum  ;      
    binsize     = x.binsize  ;      
    binsize_2   = x.binsize_2;
    
    nentries    = new vector<int>(*(x.nentries));
    abscissas   = new vector<T>(*(x.abscissas) );
    abscissas2  = new vector<T>(*(x.abscissas2));
    data        = new vector<T>(*(x.data)      );
    data2       = new vector<T>(*(x.data2)     );
    errors      = new vector<T>(*(x.errors)    );

  }
  
  return *this;

}
  
template<class T>
T & Histogram<T>::operator[](int i)
{
  return (*data)[i];
}

template<class T>
Histogram<T> const & Histogram<T>::operator+=(Histogram<T> const & x)
{
  std::transform(data->begin(), data->end(),
                 x.data->begin(), data->begin(), plus<T>());

  std::transform(data2->begin(), data2->end(),
                 x.data2->begin(), data2->begin(), plus<T>());

  std::transform(errors->begin(), errors->end(),
                 x.errors->begin(), errors->begin(), plus<T>());

  std::transform(nentries->begin(), nentries->end(),
                 x.nentries->begin(), nentries->begin(), plus<int>());

  return *this;
}

template<class T>
Histogram<T> const & Histogram<T>::operator-=(Histogram<T> const & x)
{
  std::transform(data->begin(), data->end(),
                 x.data->begin(), data->begin(), minus<T>());

  std::transform(data2->begin(), data2->end(),
                 x.data2->begin(), data2->begin(), plus<T>());

  std::transform(errors->begin(), errors->end(),
                 x.errors->begin(), errors->begin(), plus<T>());

  std::transform(nentries->begin(), nentries->end(),
                 x.nentries->begin(), nentries->begin(), plus<int>());

  return *this;
}

template<class T>
Histogram<T> const & Histogram<T>::operator*=(Histogram<T> const & x)
{
  vector<T> tmp1(*(data));
  vector<T> tmp2(*(x.data));

  std::transform(data->begin(), data->end(),
                 x.data->begin(), data->begin(), multiplies<T>());
  std::transform(data2->begin(), data2->end(),
                 x.data2->begin(), data2->begin(), multiplies<T>());
  std::transform(errors->begin(), errors->end(),
                 tmp2.begin(), tmp2.begin(), multiplies<T>());
  std::transform(x.errors->begin(), x.errors->end(),
                 tmp1.begin(), tmp1.begin(), multiplies<T>());
  std::transform(tmp1.begin(), tmp1.end(),
                 tmp2.begin(), errors->begin(), plus<T>());
  std::transform(nentries->begin(), nentries->end(),
                 x.nentries->begin(), nentries->begin(), plus<int>());

  return *this;
}

template<class T>
Histogram<T> const & Histogram<T>::operator/=(Histogram<T> const & x)
{
  clear();
  
  return *this;
}

template<class T>
void Histogram<T>::save(const char *file)
{
  ofstream fout( file );

  fout << "#name: " << histname << endl;
  fout << "#type: matrix" << endl;
  fout << "#rows: " << nbins << endl;
  fout << "#columns: 3" << endl;

  for (int i=0; i<nbins; i++) {

    fout << (*abscissa)[ i ] << ' '
         << (*data)[ i ]     << ' '
         << (*errors)[ i ]   << endl;

  }
  
  fout.close();
}

template<class T>
Histogram<T> const & operator+(Histogram<T> const & h1,
                               Histogram<T> const & h2)
{
  Histogram<T> * result = new Histogram<T>(h1);

  *result += h2;
  
  return *result;
}

template<class T>
Histogram<T> const & operator*(Histogram<T> const & h1,
                               Histogram<T> const & h2)
{
  Histogram<T> * result = new Histogram<T>(h1);

  *result *= h2;
  
  return *result;
}

template<class T, class CTE>
Histogram<T> const & operator*(CTE const & v,
                               Histogram<T> const & h2)
{
  Histogram<T> * result = new Histogram<T>(h2);
  T x = static_cast<T>( v );
  
  result->scale( x );
        
  return *result;
}

template<class T>
Histogram<T> const & convolute(Histogram<T> & h1,
                               Histogram<T> & h2)
{
  Histogram<T> * result = new Histogram<T>( h1 );
    
  const int n1 = h1.get_nbins();
  const int n2 = h2.get_nbins();
  register int i;
  register int j;
  T x1;
  T y1;
  T x2;
  T y2;
  T x;
  T y;
  
  result->clear();
  
  for(i=0; i<n1; i++) {
    
    x1 = h1.get_center( i );
    y1 = h1.get_content( i );
    
    for(j=0; j<n2; j++) {
      
      x2 = h2.get_center( j );
      y2 = h2.get_content( j );
      
      x = x1 + x2;
      y = y1 * y2;

      result->fill(x, y, 0.);
        
    }

  }

  return *result;
}


// Local Variables:
// mode:c++
// mode:font-lock
// End:
