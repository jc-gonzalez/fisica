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
 *   Declare Histogram class
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

#ifndef HISTOGRAM_H
#define HISTOGRAM_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <vector>
#include <numeric>
#include <algorithm>
#include <string>

#include <math.h>

#include <iostream>
#include <fstream>
#include <iomanip>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   nones
//------------------------------------------------------------

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

//======================================================================
// Class: Histogram
//======================================================================
template<typename T>
class Histogram {
  
public:

    /// constructor
    Histogram(std::string hname, int n, T xmin, T xmax);
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

    /// fill the histogram from data arrays
    void              fillFromData(std::vector<T> x,
                                   std::vector<T> w = std::vector<T>(), bool hasWeights = true);

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

    /// Iterator
    typedef typename std::vector<T>::iterator DIter;
    typedef typename std::vector<int>::iterator NIter;
    
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
    int                nbins;

    /// minimum and maximum of histogram
    T                  minimum;
    T                  maximum;

    /// size of bin, and size of half bin
    T                  binsize;
    T                  binsize_2;
  
    /// x coordinates of bins (center of bin)
    std::vector<T>     abscissas;

    /// x coordinates of bins, squared
    std::vector<T>     abscissas2;

    /// number of entries per bin
    std::vector<int>   nentries;

    /// number of entries per bin
    std::vector<T>     data;

    /// number of entries per bin
    std::vector<T>     data2;

    /// number of errors per bin
    std::vector<T>     errors;

    /// number of entries in the histogram (calculated on demand)
    int                entries;

    /// integrated sum of values (calculated on demand)
    T                  sum;

    /// RMS of histogram (calculated on demand)
    T                  rms;

    /// MEAN of the histogram (calculated on demand)
    T                  mean;

    /// MEAN of the histogram (calculated on demand)
    T                  variance;

    /// MEAN of the histogram (calculated on demand)
    T                  stdev;

    /// MEAN of the histogram (calculated on demand)
    T                  sigma;

    /// MEDIAN of the histogram (calculated on demand)
    T                  median;

    /// MAX : maximum of the histogram
    T                  max_value;

    /// XMAX : maximum of the histogram
    T                  xmax_value;

    /// XMAX : maximum of the histogram
    int                imax_value;

    /// NAME of the histogram
    std::string        histname;

    /// zero (one) value for initialization of vectors
    T                  zero, one;
};

}

#endif // HISTOGRAM_H
