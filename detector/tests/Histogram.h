//=++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//=
//= template class Histogram
//=
//= Uses STL functors and containers
//=
//= @file        Histogram.h
//= @desc        Template class Histogram
//= @author      J C Gonzalez
//= @date        Thu Nov 25 17:31:10 MET 1999
//= @email       gonzalez@mppmu.mpg.de 
//=
//= Kopyleft (k) 1999, 2000   J.C.Gonzalez (gonzalez@mppmu.mpg.de)
//=----------------------------------------------------------------------
//= $RCSfile$
//= $Revision$
//= $Author$ 
//= $Date$
//=----------------------------------------------------------------------

// @T \newpage

//=============================================================
//!@section Source code of |Histogram.h|.

/*!@"

  The purpose of this template is to provide of a simple way of using
  histograms of data from a C++ program, without the need of using a
  complicated C++ data library.

  @"*/

//=------------------------------------------------------------
//!@## Identifier for the class.

//!@{

#ifndef __HISTOGRAM_CLASS__
#define __HISTOGRAM_CLASS__

//!@}

//!@{

#include <vector>
#include <numeric>
#include <algorithm>
#include <string>

#include <math.h>

#include <iostream>
#include <fstream>
#include <iomanip>

//!@}

//=------------------------------------------------------------
//!@## Histogram: Interface.

//!@{

template<class T>
class Histogram
{
  
public:

  /// constructor
  Histogram(string hname,int n, T xmin, T xmax);
  Histogram();

  /// destructor
  ~Histogram();

  /// copy constructor
  Histogram(Histogram<T> const & x);
  
  /// fill the histogram
  void              fill(T x);
  
  /// fill the histogram
  void              fill(T x, T w);
  
  /// fill the histogram
  void              fill(T x, T w, T e);

  /// fill the histogram
  void              fill(int i, T w);
  
  /// fill the histogram
  void              fill(int i, T w, T e);

  /// fill the histogram
  void              fillall(T w);

  /// scale the histogram
  void              scale(T w);

  /// get number of bins
  const int         get_nbins(void);
  
  /// get bin size
  const T           get_binsize(void);
  
  /// get maximum of histogram
  const T           get_upper(void);
  
  /// get minimum of histogram
  const T           get_lower(void);
  
  /// get number of bin [x1,x2] with  x1 < x < x2
  const int         get_nbin(T& x);
  
  /// get number of bin i-th
  const T           get_upper(int i);
  
  /// get number of bin i-th
  const T           get_lower(int i);
  
  /// get number of bin i-th
  const T           get_center(int i);
  
  /// get content of bin i-th
  const T           get_content(int i);

  /// get value of bin i-th
  const int         get_entries(int i);

  /// get error of bin i-th
  const T           get_error(int i);

  /// get SUM          (calculated on demand)
  const T           get_sum(void);
  
  /// get ENTRIES      (calculated on demand)
  const int         get_entries(void);
  
  /// get MEAN         (calculated on demand)
  const T           get_mean(void);

  /// get VARIANCE     (calculated on demand)
  const T           get_variance(void);

  /// get STD.DEV      (calculated on demand)
  const T           get_stdev(void);

  /// get SIGMA        (calculated on demand)
  const T           get_sigma(void);
  
  /// get MEDIAN       (calculated on demand)
  const T           get_median(void);
   
  /// get RMS          (calculated on demand)
  const T           get_rms(void);

  /// get MAX
  const T           get_max(void);
   
  /// get XMAX
  const T           get_xmax(void);
   
  /// get IMAX
  const int         get_imax(void);
   
  /// get name of histogram
  const char*       get_name(void);
  
  /// get name of histogram
  void              set_name(const char* newname);
  
  /// clears the contents of the histogram
  void              clear(void);
  
  /// overloaded / : divides two histograms
  void              save(const char *file, const char *varname);

  /// get content of bin i-th
  T &               operator[](int i);

  /// overloaded assignment operator
  Histogram<T> const & operator=(Histogram<T> const & x);

  /// overloaded + : adds two histograms
  Histogram<T> const & operator+=(Histogram<T> const & toadd);
  
  /// overloaded - : substracts two histograms
  Histogram<T> const & operator-=(Histogram<T> const & toadd);
  
  /// overloaded * : multiplies two histograms
  Histogram<T> const & operator*=(Histogram<T> const & toadd);
  
  /// overloaded / : divides two histograms
  Histogram<T> const & operator/=(Histogram<T> const & toadd);

private:

  /// number of bins
  int           nbins;

  /// minimum and maximum of histogram
  T             minimum;
  T             maximum;

  /// size of bin, and size of half bin
  T             binsize;
  T             binsize_2;
  
  /// x coordinates of bins (center of bin)
  vector<T>     abscissas;

  /// x coordinates of bins, squared
  vector<T>     abscissas2;

  /// number of entries per bin
  vector<int>   nentries;

  /// number of entries per bin
  vector<T>     data;

  /// number of entries per bin
  vector<T>     data2;

  /// number of errors per bin
  vector<T>     errors;

  /// number of entries in the histogram (calculated on demand)
  int           entries;

  /// integrated sum of values (calculated on demand)
  T             sum;

  /// RMS of histogram (calculated on demand)
  T             rms;

  /// MEAN of the histogram (calculated on demand)
  T             mean;

  /// MEAN of the histogram (calculated on demand)
  T             variance;

  /// MEAN of the histogram (calculated on demand)
  T             stdev;

  /// MEAN of the histogram (calculated on demand)
  T             sigma;

  /// MEDIAN of the histogram (calculated on demand)
  T             median;

  /// MAX : maximum of the histogram
  T             max_value;

  /// XMAX : maximum of the histogram
  T             xmax_value;

  /// XMAX : maximum of the histogram
  int           imax_value;

  /// NAME of the histogram
  string        histname;

  /// zero (one) value for initialization of vectors
  T zero, one;
};

//!@}

// @T \newpage

//=------------------------------------------------------------
//!@## Histogram: Implementation.

//!-----------------------------------------------------------
// @name Histogram<T>::Histogram
//                                                           
// @desc Constructor
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T>::Histogram()
{
  zero = static_cast<T>(0);
  one  = static_cast<T>(1);

  nbins      = 0;
  maximum    = zero;
  minimum    = zero;
  binsize    = zero;
  binsize_2  = zero;
    
  histname   = "";
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::Histogram
//                                                           
// @desc Constructor with parameters
//
// @var    hname  Name of the histogram
// @var    n      Number of bins
// @var    xmin   Lower edge of the histogram  
// @var    xmax   Upper edge of the histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T>::Histogram(string hname, int n, T xmin, T xmax) 
{
  zero = static_cast<T>(0);
  one  = static_cast<T>(1);

  nbins   = n;
  maximum = xmax;
  minimum = xmin;

  binsize   = (maximum - minimum) /  static_cast<T>(nbins);
  binsize_2 = binsize / static_cast<T>(2);
    
  histname = hname;

  /*
    nentries.reserve( n );
    abscissas.reserve( n );
    abscissas2.reserve( n );
    data.reserve( n );
    data2.reserve( n );
    errors.reserve( n );
    
    T x;
    int i;
    for (i=0, x=minimum+binsize_2; i<n; i++, x+=binsize)
    abscissas[ i ] = x;
    
    std::transform(abscissas.begin(), abscissas.end(),
    abscissas.begin(), abscissas2.begin(), multiplies<T>());
  */
  /*  
  nentries.reserve( n );
  abscissas.reserve( n );
  abscissas2.reserve( n );
  data.reserve( n );
  data2.reserve( n );
  errors.reserve( n );

  T x, x2;
  int i;
  for (i=0, x=minimum+binsize_2; i<n; i++, x+=binsize) {

    nentries[i] = 0;
    data[i] = zero;
    data2[i] = zero;
    errors[i] = zero;

    abscissas[i] = x;

    x2 = x*x;
    abscissas2[i]= x2;

  }
  
  //clear();
  */
  T x, x2;
  int i;
  for (i=0, x=minimum+binsize_2; i<n; i++, x+=binsize) {

    nentries.push_back( 0 );
    data.push_back( zero );
    data2.push_back( zero );
    errors.push_back( zero );

    abscissas.push_back( x );

    x2 = x*x;
    abscissas2.push_back( x2 );

  }
  
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::Histogram
//                                                           
// @desc Copy constructor
//
// @var    x      Source histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T>::Histogram(Histogram<T> const & x)
{
  nbins       = x.nbins     ;        
  minimum     = x.minimum   ;      
  maximum     = x.maximum   ;      
  binsize     = x.binsize   ;      
  binsize_2   = x.binsize_2 ;
  
  nentries    = x.nentries  ;
  abscissas   = x.abscissas ;
  abscissas2  = x.abscissas2;
  data        = x.data      ;
  data2       = x.data2     ;
  errors      = x.errors    ;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::Histogram
//                                                           
// @desc Destructor
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T>::~Histogram()
{
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_nbins
//                                                           
// @desc Gives number of bins
//
// @return   Number of bins
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const int Histogram<T>::get_nbins(void)
{
  return nbins;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_binsize
//                                                           
// @desc Gives bin size 
//
// @return   Bin size
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const T Histogram<T>::get_binsize(void)
{
  return binsize;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_upper
//                                                           
// @desc     Gives upper edge of histogram
//
// @return   Upper edge of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const T Histogram<T>::get_upper(void)
{
  return maximum;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_lower
//                                                           
// @desc     Gives lower edge of histogram
//
// @return   Lower edge of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const T Histogram<T>::get_lower(void)
{
  return minimum;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_nbin
//                                                           
// @desc     Calculates number of bin where x is found
//
// @return   Number of bin with x
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const int Histogram<T>::get_nbin(T& x)
{
  return static_cast<int>(floor( (x - minimum) / binsize ));
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_upper
//                                                           
// @desc     Calculates upper edge of bin i 
//
// @return   Upper edge of bin i 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const T Histogram<T>::get_upper(int i)
{
  return abscissas[ i ] + binsize_2; 
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_lower
//                                                           
// @desc     Calculates lower edge of bin i
//
// @return   Lower edge of bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const T Histogram<T>::get_lower(int i)
{
  return abscissas[ i ] - binsize_2;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_center
//                                                           
// @desc     Gives center of bin i 
//
// @return   Center of bin i 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const T Histogram<T>::get_center(int i)
{
  return abscissas[ i ];
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_content
//                                                           
// @desc     Gives content of bin i
//
// @return   Content of bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const T Histogram<T>::get_content(int i)
{
  return data[ i ];
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_entries
//                                                           
// @desc     Gives number of entries in bin i
//
// @return   Number of entries in bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const int Histogram<T>::get_entries(int i)
{
  return nentries[ i ];
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_error
//                                                           
// @desc     Gives error in bin i
//
// @return   Error in bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const T Histogram<T>::get_error(int i)
{
  return errors[ i ];
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_sum
//                                                           
// @desc     Calculates integral of the histogram
//
// @return   Integral of the histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_sum(void)
{
  return std::accumulate(data.begin(), data.end(), zero);
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_entries
//                                                           
// @desc     Calculates number of entries in histogram
//
// @return   Number of entries in histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const int Histogram<T>::get_entries(void)
{
  return std::accumulate(nentries.begin(), nentries.end(), 0);
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_mean
//                                                           
// @desc     Calculates MEAN of histogram 
//
// @return   MEAN of histogram 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_mean(void)
{
  mean = (std::inner_product(data.begin(), data.end(),
                             abscissas.begin(), zero) / get_sum());
  return mean;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_variance
//                                                           
// @desc     Calculates Variance of histogram
//
// @return   Variance of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_variance(void)
{
  T tmpmean = get_mean();
  
  variance = (std::inner_product(data.begin(), data.end(),
                                 abscissas2.begin(), zero) /
              get_sum()) - (tmpmean * tmpmean);
  
  return variance;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_stdev
//                                                           
// @desc     Calculates Standard Deviation of histogram   
//
// @return   Std.Deviation of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_stdev(void)
{
  stdev = sqrt( get_variance() );
  
  return stdev;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_sigma
//                                                           
// @desc     Calculates SIGMA of histogram  
//
// @return   SIGMA of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_sigma(void)
{
  return ( get_stdev() );
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_median
//                                                           
// @desc     Calculates MEDIAN of histogram 
//
// @return   MEDIAN of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_median(void)
{
  T tmpsum = get_sum();
  T half   = tmpsum / static_cast<T>(2);
  T cumsum = zero;
  T xa     = minimum - binsize;
  T xb     = minimum;

  T * first = data.begin();
  T * last  = data.end();

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
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_rms
//                                                           
// @desc     Calculates RMS of histogram
//
// @return   RMS of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_rms(void)
{
  return zero;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_max
//                                                           
// @desc     Gives maximum content of bins, and calc. pos. of maximum
//
// @return   Maximum content of bins
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_max(void)
{
  max_value  = zero;
  xmax_value = minimum;
  imax_value = 0;
  
  for (int i=0; i<nbins; i++) {
    if ( data[i] > max_value ) {
      max_value  = data[i];
      xmax_value = abscissas[i];
      imax_value = i;
    }
  }

  return max_value;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_xmax
//                                                           
// @desc     Gives center of bin with maximum content 
//
// @return   Center of bin with maximum content 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const T Histogram<T>::get_xmax(void)
{
  return xmax_value;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_imax
//                                                           
// @desc     Gives number of bin with maximum content
//
// @return   Number of bin with maximum content
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
const int Histogram<T>::get_imax(void)
{
  return imax_value;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin with x, with weight 1
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::fill(T x)
{
  if ( (x > minimum) && (x < maximum) ) {
    int i = get_nbin( x );
    nentries[ i ] += 1;
    data[ i ]   = data[ i ] + one;
    data2[ i ]  = data2[ i ] + one;
  }
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin with x, with weight w
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::fill(T x, T w)
{
  if ( (x > minimum) && (x < maximum) ) {
    int i = get_nbin( x );
    nentries[ i ] += 1;
    data[ i ]   = data[ i ] + w;
    data2[ i ]  = data2[ i ] + (w * w);
  }
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin with x, with weight w and error e 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::fill(T x, T w, T e)
{
  if ( (x > minimum) && (x < maximum) ) {
    int i = get_nbin( x );
    nentries[ i ] += 1;
    data[ i ]   += w;
    data2[ i ]  += (w * w);
    errors[ i ] += e;
  }
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin i with weight w
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::fill(int i, T w)
{
  if ( (i >= 0) && (i < nbins) ) {
    nentries[ i ] += 1;
    data[ i ]   = data[ i ] + w;
    data2[ i ]  = data2[ i ] + (w * w);
  }
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin i with weight w and error e
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::fill(int i, T w, T e)
{
  if ( (i >= 0) && (i < nbins) ) {
    nentries[ i ] += 1;
    data[ i ]   = data[ i ] + w;
    data2[ i ]  = data2[ i ] + (w * w);
    errors[ i ] = errors[ i ] + e;
  }
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::fillall
//                                                           
// @desc     Fill all the histogram with a constant value
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::fillall(T w)
{
  for (int i=0; i<nbins; i++) {
    nentries[ i ] = 1;
    data[ i ]   = w;
    data2[ i ]  = (w * w);
    errors[ i ] = zero;
  }
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::scale
//                                                           
// @desc    Scale histogram by a constant factor
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::scale(T w)
{
  for (int i=0; i<nbins; i++) {
    data[ i ]   *= w;
    data2[ i ]  *= (w * w);
    errors[ i ] *= w;
  }
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::clear
//                                                           
// @desc     Wipe out data in histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::clear(void)
{
  std::fill(data.begin(), data.end(), zero);
  std::fill(data2.begin(), data2.end(), zero);
  std::fill(errors.begin(), errors.end(), zero);
  std::fill(nentries.begin(), nentries.end(), 0);
  
  entries = 0;
  sum     = zero;
  rms     = zero;
  mean    = zero;
  median  = zero;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::get_name
//                                                           
// @desc     Gives description of histogram 
//
// @return   Description of histogram 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline const char* Histogram<T>::get_name(void)
{
  return histname;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::set_name
//                                                           
// @desc     Change description of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
inline void Histogram<T>::set_name(const char* newname)
{
  histname = newname;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::operator=
//                                                           
// @desc     Copy operator
//
// @return   Reference to resulting histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T> const & Histogram<T>::operator=(Histogram<T> const & x)
{
  if (this != &x) {
    
    nentries    = x.nentries  ;
    abscissas   = x.abscissas ;
    abscissas2  = x.abscissas2;
    data        = x.data      ;
    data2       = x.data2     ;
    errors      = x.errors    ;
      
    nbins       = x.nbins    ;        
    minimum     = x.minimum  ;      
    maximum     = x.maximum  ;      
    binsize     = x.binsize  ;      
    binsize_2   = x.binsize_2;

  }
  
  return *this;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::operator[]
//                                                           
// @desc   Returns content of bin i
//
// @return   Content of bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
T & Histogram<T>::operator[](int i)
{
  return data[i];
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::operator+=
//                                                           
// @desc   Adds to this this histogram another one
//
// @var    x    Histogram to be added
//
// @return    *this
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T> const & Histogram<T>::operator+=(Histogram<T> const & x)
{
  std::transform(data.begin(), data.end(),
                 x.data.begin(), data.begin(), plus<T>());

  std::transform(data2.begin(), data2.end(),
                 x.data2.begin(), data2.begin(), plus<T>());

  std::transform(errors.begin(), errors.end(),
                 x.errors.begin(), errors.begin(), plus<T>());

  std::transform(nentries.begin(), nentries.end(),
                 x.nentries.begin(), nentries.begin(), plus<int>());

  return *this;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::operator-=
//                                                           
// @desc   Substracts to this this histogram another one
//
// @var    x    Histogram acting as substractor
//
// @return    *this
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T> const & Histogram<T>::operator-=(Histogram<T> const & x)
{
  std::transform(data.begin(), data.end(),
                 x.data.begin(), data.begin(), minus<T>());

  std::transform(data2.begin(), data2.end(),
                 x.data2.begin(), data2.begin(), plus<T>());

  std::transform(errors.begin(), errors.end(),
                 x.errors.begin(), errors.begin(), plus<T>());

  std::transform(nentries.begin(), nentries.end(),
                 x.nentries.begin(), nentries.begin(), plus<int>());

  return *this;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::operator*=
//                                                           
// @desc   Multiplies this histogram by another one
//
// @var    x    Histogram acting as factor
//
// @return    *this
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T> const & Histogram<T>::operator*=(Histogram<T> const & x)
{
  vector<T> tmp1(*(data));
  vector<T> tmp2(*(x.data));

  std::transform(data.begin(), data.end(),
                 x.data.begin(), data.begin(), multiplies<T>());
  std::transform(data2.begin(), data2.end(),
                 x.data2.begin(), data2.begin(), multiplies<T>());
  std::transform(errors.begin(), errors.end(),
                 tmp2.begin(), tmp2.begin(), multiplies<T>());
  std::transform(x.errors.begin(), x.errors.end(),
                 tmp1.begin(), tmp1.begin(), multiplies<T>());
  std::transform(tmp1.begin(), tmp1.end(),
                 tmp2.begin(), errors.begin(), plus<T>());
  std::transform(nentries.begin(), nentries.end(),
                 x.nentries.begin(), nentries.begin(), plus<int>());

  return *this;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::operator/=
//                                                           
// @desc   Divides this histogram by another one
//
// @var    x    Histogram acting as divisor
//
// @return    *this
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T> const & Histogram<T>::operator/=(Histogram<T> const & x)
{
  clear();
  
  return *this;
}
//!@}


//!-----------------------------------------------------------
// @name Histogram<T>::save
//                                                           
// @desc   Save histogram in ASCII file (Octave format)
//
// @var    file      Filename
// @var    varname   Name of data array in file
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void Histogram<T>::save(const char *file, const char *varname)
{
  ofstream fout( file );

  fout << "#name: " << varname << endl;
  fout << "#type: matrix" << endl;
  fout << "#rows: " << nbins << endl;
  fout << "#columns: 3" << endl;

  for (int i=0; i<nbins; i++) {

    fout << abscissas[ i ] << ' '
         << data[ i ]      << ' '
         << errors[ i ]    << endl;

  }
  
  fout.close();

}
//!@}


//!-----------------------------------------------------------
// @name operator+
//                                                           
// @desc   Sums two histograms bin by bin
//
// @var    h1    First histogram
// @var    h2    Second histogram
//
// @return       Reference to resulting histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T> const & operator+(Histogram<T> const & h1,
                               Histogram<T> const & h2)
{
  Histogram<T> result( h1 );

  result += h2;
  
  return result;
}
//!@}


//!-----------------------------------------------------------
// @name operator*
//                                                           
// @desc   Multiplies two histograms bin by bin
//
// @var    h1    First histogram
// @var    h2    Second histogram
//
// @return       Reference to resulting histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
Histogram<T> const & operator*(Histogram<T> const & h1,
                               Histogram<T> const & h2)
{
  Histogram<T> result( h1 );

  result *= h2;
  
  return result;
}
//!@}


//!-----------------------------------------------------------
// @name operator*
//                                                           
// @desc   Multiplies a histogram by a constant
//
// @var    k     Constant
// @var    h2    Histogram to multiply
//
// @return       Reference to resulting histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T, class CTE>
Histogram<T> const & operator*(CTE const & k,
                               Histogram<T> const & h2)
{
  Histogram<T> result( h2 );
  T x = static_cast<T>( k );
  
  result.scale( x );
        
  return result;
}
//!@}


//!-----------------------------------------------------------
// @name convolute
//                                                           
// @desc Calculates the convolution of two histograms
//
// @var   h1    Histogram of values
// @var   h2    Histogram of responses
//
// @return      Reference to resulting histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
// @function  

//!@{
template<class T>
void convolute(Histogram<T> & h1, Histogram<T> & h2, Histogram<T> & h3)
{
    
  const int n1 = h1.get_nbins();
  const int n2 = h2.get_nbins();
  int i;
  int j;
  T x1;
  T y1;
  T x2;
  T y2;
  T x;
  T y;
  
  h3.clear();

  for(i=0; i<n1; i++) {
    
    x1 = h1.get_center( i );
    y1 = h1.get_content( i );
    
    for(j=0; j<n2; j++) {
      
      x2 = h2.get_center( j );
      y2 = h2.get_content( j );
      
      x = x1 + x2;
      y = y1 * y2;

      h3.fill(x, y, static_cast<T>(0));
        
    }

  }

}
//!@}


//=------------------------------------------------------------
//!@## Close definition.

//!@{

#endif // __HISTOGRAM_CLASS__

//!@}

//=------------------------------------------------------------
//!@## Log of this file.

//!@{

/*
 *$Log$
 * Revision 1.1  2000/01/30  08:48:40  gonzalez
 * *** empty log message ***
 *
 *
 */

//!@}

//!@{
// Local Variables:
// mode:c++
// mode:font-lock
// End:
//!@}
//=EOF
