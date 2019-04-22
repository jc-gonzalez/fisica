/******************************************************************************
 * File:    histogram.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.histogram
 *
 * Last update:  2.0
 *
 * Date:    2019/03/13
 *
 * Author:  J C Gonzalez
 *
 * Copyright (C) 2019 by J C Gonzalez
 *_____________________________________________________________________________
 *
 * Topic: General Information
 *
 * Purpose:
 *   Declare DockerMng class
 *
 * Created by:
 *   J C Gonzalez
 *
 * Status:
 *   Prototype
 *
 * Dependencies:
 *   Component
 *
 * Files read / modified:
 *   none
 *
 * History:
 *   See <Changelog>
 *
 * About: License Conditions
 *   See <License>
 *
 ******************************************************************************/

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   nones
//------------------------------------------------------------
#include "histogram.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

//!-----------------------------------------------------------
// @name Histogram<T>::Histogram
//                                                           
// @desc Constructor
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
Histogram<T>::Histogram()
{
    // initialize zero and one constants
    zero = static_cast<T>(0);
    one  = static_cast<T>(1);

    // initialize parameters of the histogram
    nbins      = 0;
    maximum    = zero;
    minimum    = zero;
    binsize    = zero;
    binsize_2  = zero;
    
    histname   = "";
}


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
template<typename T>
Histogram<T>::Histogram(std::string hname, int n, T xmin, T xmax) 
{
    // initialize zero and one constants
    zero = static_cast<T>(0);
    one  = static_cast<T>(1);

    // initialize parameters of the histogram (user input)
    nbins   = n;
    maximum = xmax;
    minimum = xmin;

    // set bin size
    binsize   = (maximum - minimum) /  static_cast<T>(nbins);
    binsize_2 = binsize / static_cast<T>(2);

    // set histogram name
    histname = hname;

    // create empty (zero values) space for the different vectors of the
    // histogram (nentries, data, data^2, errors, abscissas and abscissas^2
    T x, x2;
  
    for (int i = 0, x = minimum + binsize_2; i < n; i++, x += binsize) {
        /*
          nentries.push_back( 0 );
          data.push_back( zero );
          data2.push_back( zero );
          errors.push_back( zero );
        */
        abscissas.push_back( x );
        x2 = x*x;
        abscissas2.push_back( x2 );
    }
    
    data.insert(data.begin(), nbins, zero);
    data2.insert(data2.begin(), nbins, zero);
    errors.insert(errors.begin(), nbins, zero);
    nentries.insert(nentries.begin(), nbins, 0);
  
}


//!-----------------------------------------------------------
// @name Histogram<T>::Histogram
//                                                           
// @desc Copy constructor
//
// @var    x      Source histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
Histogram<T>::Histogram(Histogram<T> const & x)
{
    // copy parameters of referenced histogram into this one
    nbins       = x.nbins     ;        
    minimum     = x.minimum   ;      
    maximum     = x.maximum   ;      
    binsize     = x.binsize   ;      
    binsize_2   = x.binsize_2 ;
  
    // copy vectors of referenced histogram into this one
    nentries    = x.nentries  ;
    abscissas   = x.abscissas ;
    abscissas2  = x.abscissas2;
    data        = x.data      ;
    data2       = x.data2     ;
    errors      = x.errors    ;
}


//!-----------------------------------------------------------
// @name Histogram<T>::Histogram
//                                                           
// @desc Destructor
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
Histogram<T>::~Histogram()
{
    // automatic destruction: real containers (and not pointer to
    // containers) are stored in this class, and therefore the STL takes
    // care of the destruction
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_nbins
//                                                           
// @desc Gives number of bins
//
// @return   Number of bins
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const int Histogram<T>::get_nbins(void)
{
    // returns number of bins in this histogram
    return nbins;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_binsize
//                                                           
// @desc Gives bin size 
//
// @return   Bin size
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const T Histogram<T>::get_binsize(void)
{
    // returns bin width
    return binsize;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_upper
//                                                           
// @desc     Gives upper edge of histogram
//
// @return   Upper edge of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const T Histogram<T>::get_upper(void)
{
    // return upper edge of histogram
    return maximum;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_lower
//                                                           
// @desc     Gives lower edge of histogram
//
// @return   Lower edge of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const T Histogram<T>::get_lower(void)
{
    // return lower edge of histogram
    return minimum;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_nbin
//                                                           
// @desc     Calculates number of bin where x is found
//
// @return   Number of bin with x
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const int Histogram<T>::get_nbin(T& x)
{
    // returns integer number i of bin that contains the abscissa value x
    return static_cast<int>(floor( (x - minimum) / binsize ));
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_upper
//                                                           
// @desc     Calculates upper edge of bin i 
//
// @return   Upper edge of bin i 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const T Histogram<T>::get_upper(int i)
{
    // returns upper limit of i-th bin
    return abscissas[ i ] + binsize_2; 
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_lower
//                                                           
// @desc     Calculates lower edge of bin i
//
// @return   Lower edge of bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const T Histogram<T>::get_lower(int i)
{
    // returns lower limit of i-th bin
    return abscissas[ i ] - binsize_2;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_center
//                                                           
// @desc     Gives center of bin i 
//
// @return   Center of bin i 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const T Histogram<T>::get_center(int i)
{
    // returns the abscissa value of the center of i-th bin
    return abscissas[ i ];
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_content
//                                                           
// @desc     Gives content of bin i
//
// @return   Content of bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const T Histogram<T>::get_content(int i)
{
    // returns content of i-th bin
    return data[ i ];
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_entries
//                                                           
// @desc     Gives number of entries in bin i
//
// @return   Number of entries in bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const int Histogram<T>::get_entries(int i)
{
    // returns number of entries in i-th bin
    return nentries[ i ];
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_error
//                                                           
// @desc     Gives error in bin i
//
// @return   Error in bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const T Histogram<T>::get_error(int i)
{
    // returns error value for i-th bin
    return errors[ i ];
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_sum
//                                                           
// @desc     Calculates integral of the histogram
//
// @return   Integral of the histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_sum(void)
{
    // returns sum of contents of all bins in histogram
    // uses the STL function accumulate(first, last, init)
    return std::accumulate(data.begin(), data.end(), zero);
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_entries
//                                                           
// @desc     Calculates number of entries in histogram
//
// @return   Number of entries in histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const int Histogram<T>::get_entries(void)
{
    // returns total number of entries in histogram
    // uses the STL function accumulate(first, last, init)
    return std::accumulate(nentries.begin(), nentries.end(), 0);
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_mean
//                                                           
// @desc     Calculates MEAN of histogram 
//
// @return   MEAN of histogram 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_mean(void)
{
    // returns weighted mean of histogram
    // the weigths are the contents of each bin
    // uses the STL function inner_product(first, last, first2, init)
    // it needs the sum of the contents

    T tmpsum = get_sum();

    if (tmpsum == zero) {
        mean = zero;
    } else { 
        // saves computed mean in private member
        mean = (std::inner_product(data.begin(), data.end(),
                                   abscissas.begin(), zero) / get_sum());
    }
  
    return mean;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_variance
//                                                           
// @desc     Calculates Variance of histogram
//
// @return   Variance of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_variance(void)
{
    // returns variance of the histogram
    // uses the STL function inner_product(first, last, first2, init)

    T tmpsum = get_sum();

    if (tmpsum == zero) {
        variance = zero;
    } else {
        // it needs the mean of the histogram
        T tmpmean = get_mean();

        // saves computed variance in private member
        variance = (std::inner_product(data.begin(), data.end(),
                                       abscissas2.begin(), zero) /
                    tmpsum) - (tmpmean * tmpmean);
    }
  
    return variance;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_stdev
//                                                           
// @desc     Calculates Standard Deviation of histogram   
//
// @return   Std.Deviation of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_stdev(void)
{
    // returns standard deviation of the histogram
    // it calculates st.dev. as sqrt( variance )

    // saves computed st.dev. in private member
    stdev = sqrt( get_variance() );
  
    return stdev;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_sigma
//                                                           
// @desc     Calculates SIGMA of histogram  
//
// @return   SIGMA of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_sigma(void)
{
    // returns sigma == standard deviation of the histogram
    return ( get_stdev() );
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_median
//                                                           
// @desc     Calculates MEDIAN of histogram 
//
// @return   MEDIAN of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_median(void)
{
    // initialize intermediate values
    T tmpsum = get_sum();                   // sum of all values
    T half   = tmpsum / static_cast<T>(2);  // half the sum
    T cumsum = zero;                        
    T xa     = minimum - binsize;           // lower limit to check
    T xb     = minimum;                     // upper limit to check

    DIter first = data.begin();               // iterator, runs over the data
    DIter last  = data.end();                 // iterator, end of data

    // runs over all the data
    while (first != last) {

        // set interval to check
        xa = xb;
        xb += binsize;

        // add content of this new bin to cumsum
        cumsum += (*first);

        // if cumsum > half the total sum, in this bin we reach the median
        if ( cumsum > half )
            break;  // and exit this loop

        // go to next bin
        first++;
    }

    // get value of cumsum before this bin was added
    cumsum -= (*first);

    // interpolate (linear) the value of the median
    // and save the value in a private member
    median = xa + (xb - xa) * (half - cumsum) / (*first); 

    return  median;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_rms
//                                                           
// @desc     Calculates RMS of histogram
//
// @return   RMS of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_rms(void)
{
    // [TODO] THIS ROUTINE STILL EMPTY
    return zero;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_max
//                                                           
// @desc     Gives maximum content of bins, and calc. pos. of maximum
//
// @return   Maximum content of bins
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_max(void)
{
    // this routine searches for the position of the maximum in
    // the histogram
    // the maximum content, the center of the bin where it is
    // found and the bin number is then saved to private members

    // THIS ROUTINE HAS TO BE USED BEFORE get_xmax OR get_imax

    // initialize variables
    max_value  = zero;
    xmax_value = zero;
    imax_value = 0;

    // loop over bins
    for (int i = 0; i < nbins; i++) {

        // set the values, in case a bigger content is found
        if ( data[i] > max_value ) {
            max_value  = data[i];
            xmax_value = abscissas[i];
            imax_value = i;
        }
    
    }

    // return maximum content
    return max_value;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_xmax
//                                                           
// @desc     Gives center of bin with maximum content 
//
// @return   Center of bin with maximum content 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const T Histogram<T>::get_xmax(void)
{
    // returns center of bin with maximum content

    // get_max HAS TO BE USED BEFORE THIS ROUTINE
  
    return xmax_value;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_imax
//                                                           
// @desc     Gives number of bin with maximum content
//
// @return   Number of bin with maximum content
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
const int Histogram<T>::get_imax(void)
{
    // returns number of bin with maximum content

    // get_max HAS TO BE USED BEFORE THIS ROUTINE
  
    return imax_value;
}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin with x, with weight 1
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::fill(T x)
{
    // put this value in the histogram
  
    if ( (x > minimum) && (x < maximum) ) {
        int i = get_nbin( x );
        nentries[ i ] += 1;
        data[ i ]   = data[ i ] + one;
        data2[ i ]  = data2[ i ] + one;
    }
}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin with x, with weight w
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::fill(T x, T w)
{
    // put this value in the histogram, with weigth w
  
    if ( (x > minimum) && (x < maximum) ) {
        int i = get_nbin( x );
        nentries[ i ] += 1;
        data[ i ]   = data[ i ] + w;
        data2[ i ]  = data2[ i ] + (w * w);
    }
}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin with x, with weight w and error e 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::fill(T x, T w, T e)
{
    // put this value in the histogram, with weigth w and error e
  
    if ( (x > minimum) && (x < maximum) ) {
        int i = get_nbin( x );
        nentries[ i ] += 1;
        data[ i ]   += w;
        data2[ i ]  += (w * w);
        errors[ i ] += e;
    }
}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin i with weight w
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::fill(int i, T w)
{
    // add weight w in bin i
  
    if ( (i >= 0) && (i < nbins) ) {
        nentries[ i ] += 1;
        data[ i ]   = data[ i ] + w;
        data2[ i ]  = data2[ i ] + (w * w);
    }
}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin i with weight w and error e
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::fill(int i, T w, T e)
{
    // add weight w in bin i, with error e
  
    if ( (i >= 0) && (i < nbins) ) {
        nentries[ i ] += 1;
        data[ i ]   = data[ i ] + w;
        data2[ i ]  = data2[ i ] + (w * w);
        errors[ i ] = errors[ i ] + e;
    }
}


//!-----------------------------------------------------------
// @name Histogram<T>::fill
//                                                           
// @desc    Fill bin with x, with weight w
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::fillFromData(std::vector<T> x, std::vector<T> w, bool hasWeights)
{
    // put this value in the histogram, with weigth w
    if (hasWeights) {
        for (int i = 0; i < x.size(); ++i) { this->fill(x.at(i), 1); }
    } else {
        for (int i = 0; i < x.size(); ++i) { this->fill(x.at(i), w.at(i)); }
    }
}


//!-----------------------------------------------------------
// @name Histogram<T>::fillall
//                                                           
// @desc     Fill all the histogram with a constant value
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::fillall(T w)
{
    // add weight w in all bins
  
    for (int i = 0; i < nbins; i++) {
        nentries[ i ] = 1;
        data[ i ]   = w;
        data2[ i ]  = (w * w);
        errors[ i ] = zero;
    }
}


//!-----------------------------------------------------------
// @name Histogram<T>::scale
//                                                           
// @desc    Scale histogram by a constant factor
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::scale(T w)
{
    // multiplies all the histogram by a constant factor
  
    for (int i = 0; i < nbins; i++) {
        data[ i ]   *= w;
        data2[ i ]  *= (w * w);
        errors[ i ] *= w;
    }
}


//!-----------------------------------------------------------
// @name Histogram<T>::clear
//                                                           
// @desc     Wipe out data in histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
void Histogram<T>::clear(void)
{
    /*
    // put to zero all the elements of the vectors
    // uses STL function fill(first, last, value)
    std::fill(data.begin(), data.end(), zero);
    std::fill(data2.begin(), data2.end(), zero);
    std::fill(errors.begin(), errors.end(), zero);
    std::fill(nentries.begin(), nentries.end(), 0);
    */
  
    data.clear();
    data2.clear();
    errors.clear();
    nentries.clear();

    data.insert(data.begin(), nbins, zero);
    data2.insert(data2.begin(), nbins, zero);
    errors.insert(errors.begin(), nbins, zero);
    nentries.insert(nentries.begin(), nbins, 0);

    // reset entries and calculated values
    entries = 0;
    sum     = zero;
    rms     = zero;
    mean    = zero;
    median  = zero;
}


//!-----------------------------------------------------------
// @name Histogram<T>::get_name
//                                                           
// @desc     Gives description of histogram 
//
// @return   Description of histogram 
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline const char* Histogram<T>::get_name(void)
{
    // returns description of the histogram
    return histname.c_str();
}


//!-----------------------------------------------------------
// @name Histogram<T>::set_name
//                                                           
// @desc     Change description of histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
inline void Histogram<T>::set_name(const char* newname)
{
    // sets description of the histogram
    histname = newname;
}


//!-----------------------------------------------------------
// @name Histogram<T>::operator=
//                                                           
// @desc     Copy operator
//
// @return   Reference to resulting histogram
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
Histogram<T> const & Histogram<T>::operator=(Histogram<T> const & x)
{
    // if the referenced histogram is NOT this one, then copy it 
    if (this != &x) {

        // copy vectors from referenced histogram into this one
        nentries    = x.nentries  ;
        abscissas   = x.abscissas ;
        abscissas2  = x.abscissas2;
        data        = x.data      ;
        data2       = x.data2     ;
        errors      = x.errors    ;
      
        // copy paraqmeters from referenced histogram into this one
        nbins       = x.nbins    ;        
        minimum     = x.minimum  ;      
        maximum     = x.maximum  ;      
        binsize     = x.binsize  ;      
        binsize_2   = x.binsize_2;

    }

    // returns reference to this histogram
    return *this;
}


//!-----------------------------------------------------------
// @name Histogram<T>::operator[]
//                                                           
// @desc   Returns content of bin i
//
// @return   Content of bin i
//
// @date Wed Dec  1 17:51:03 MET 1999
//------------------------------------------------------------
template<typename T>
T & Histogram<T>::operator[](int i)
{
    // shortcut: returns element i-th of data vector
    return data[i];
}


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
template<typename T>
Histogram<T> const & Histogram<T>::operator+=(Histogram<T> const & x)
{
    std::transform(data.begin(), data.end(),
                   x.data.begin(), data.begin(), std::plus<T>());

    std::transform(data2.begin(), data2.end(),
                   x.data2.begin(), data2.begin(), std::plus<T>());

    std::transform(errors.begin(), errors.end(),
                   x.errors.begin(), errors.begin(), std::plus<T>());

    std::transform(nentries.begin(), nentries.end(),
                   x.nentries.begin(), nentries.begin(), std::plus<int>());

    return *this;
}


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
template<typename T>
Histogram<T> const & Histogram<T>::operator-=(Histogram<T> const & x)
{
    std::transform(data.begin(), data.end(),
                   x.data.begin(), data.begin(), std::minus<T>());

    std::transform(data2.begin(), data2.end(),
                   x.data2.begin(), data2.begin(), std::plus<T>());

    std::transform(errors.begin(), errors.end(),
                   x.errors.begin(), errors.begin(), std::plus<T>());

    std::transform(nentries.begin(), nentries.end(),
                   x.nentries.begin(), nentries.begin(), std::plus<int>());

    return *this;
}


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
template<typename T>
Histogram<T> const & Histogram<T>::operator*=(Histogram<T> const & x)
{
    std::vector<T> tmp1(data);
    std::vector<T> tmp2(x.data);

    std::transform(data.begin(), data.end(),
                   x.data.begin(), data.begin(), std::multiplies<T>());
    std::transform(data2.begin(), data2.end(),
                   x.data2.begin(), data2.begin(), std::multiplies<T>());
    std::transform(errors.begin(), errors.end(),
                   tmp2.begin(), tmp2.begin(), std::multiplies<T>());
    std::transform(x.errors.begin(), x.errors.end(),
                   tmp1.begin(), tmp1.begin(), std::multiplies<T>());
    std::transform(tmp1.begin(), tmp1.end(),
                   tmp2.begin(), errors.begin(), std::plus<T>());
    std::transform(nentries.begin(), nentries.end(),
                   x.nentries.begin(), nentries.begin(), std::plus<int>());

    return *this;
}


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
template<typename T>
Histogram<T> const & Histogram<T>::operator/=(Histogram<T> const & x)
{
    // [TODO] THIS ROUTINE STILL DO NOTHING... WELL IT ERASES THIS HISTOGRAM
  
    clear();
  
    return *this;
}


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
template<typename T>
void Histogram<T>::save(const char *file, const char *varname)
{
    // open output file 
    std::ofstream fout( file );

    // put header (Octave format) in the file
    fout << "#name: " << varname << std::endl;
    fout << "#type: matrix" << std::endl;
    fout << "#rows: " << nbins << std::endl;
    fout << "#columns: 3" << std::endl;

    // write contents of histogram in output file
    for (int i = 0; i < nbins; i++) {

        fout << abscissas[ i ] << ' '
             << data[ i ]      << ' '
             << errors[ i ]    << std::endl;

    }

    // close output file
    fout.close();

}


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
template<typename T>
Histogram<T> const & operator+(Histogram<T> const & h1,
                               Histogram<T> const & h2)
{
    // create new intermediate histogram, from h1
    Histogram<T> result( h1 );

    // adds to intermediate histogram the histogram h2
    result += h2;

    // returns reference to new histogram
    return result;
}


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
template<typename T>
Histogram<T> const & operator*(Histogram<T> const & h1,
                               Histogram<T> const & h2)
{
    // create new intermediate histogram, from h1
    Histogram<T> result( h1 );

    // multiplies intermediate histogram by the histogram h2
    result *= h2;
  
    // returns reference to new histogram
    return result;
}


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
template<class T, class CTE>
Histogram<T> const & operator*(CTE const & k,
                               Histogram<T> const & h2)
{
    // create new intermediate histogram, from h1
    Histogram<T> result( h2 );

    // create intermediate constant value
    T x = static_cast<T>( k );

    // scale intermediate histogram with constant value
    result.scale( x );
        
    // returns reference to new histogram
    return result;
}


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
template<typename T>
void convolute(Histogram<T> & h1, Histogram<T> & h2, Histogram<T> & h3)
{
    // calculates h3 = h1 o h2, where 'o' means 'convoluted with'
  
    // get number of bins from signal and filter response
    const int n1 = h1.get_nbins();
    const int n2 = h2.get_nbins();

    // create auxiliary variables
    T x1;
    T y1;
    T x2;
    T y2;
    T x;
    T y;

    // clear target histogram
    h3.clear();

    // for each bin in histogram h1
    for (int i = 0; i < n1; i++) {

        // get center of bin i-th in h1
        x1 = h1.get_center( i );
        // and content of bin i-th in h1
        y1 = h1.get_content( i );
    
        // for each bin in histogram h2
        for (int j = 0; j < n2; j++) {
      
            // get center of bin j-th in h1
            x2 = h2.get_center( j );
            // and content of bin j-th in h1
            y2 = h2.get_content( j );

            // calculate target abscissa
            x = x1 + x2;
            // and resulting response
            y = y1 * y2;

            // put this data in the target histogram
            h3.fill(x, y, static_cast<T>(0));
        
        } // j-th bin : h2

    } // i-th bin : h1

}

template class Histogram<double>;
template class Histogram<long>;

}
