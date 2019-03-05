/////////////////////////////////////////////////////////////////
//
// CORStatfile
//
//  Created: Tue Apr 28 16:14:05 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for Statfiles
//  Notes:   
//
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt CORStatfile.hxx}

/* @text
This section shows the include file {\tt CORStatfile.hxx}
@endtext */

#ifndef CORStatfile_Class
#define CORStatfile_Class

// @subsection Include files

// @code
#include "jctypes.h"

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>

using namespace std;

#define MAXBUF 273  /* 39*7 */
#define NSUBBL 21
#define NPLONG 1041
// @endcode

// @subsection Class {\em CORStatfile}: Definition

// @code
class CORStatfile {
  
public:
  Int_t steps;          // steps in the longitudinal distribution
  Float_t *evth;          // event header
  Float_t *evte;          // event end
  Int_t *nproto;          // number of protons
  Int_t *nprotb;          // number of anti-protons
  Int_t *nneutr;          // number of neutrons
  Int_t *nneutb;          // number of 
  Int_t *nphoto;          // number of 
  Int_t *nelect;          // number of 
  Int_t *nposit;          // number of 
  Int_t *nnu;         // number of 
  Int_t *nmum;        // number of 
  Int_t *nmup;        // number of 
  Int_t *npi0;        // number of 
  Int_t *npim;        // number of 
  Int_t *npip;        // number of 
  Int_t *nk0l;        // number of 
  Int_t *nk0s;        // number of 
  Int_t *nkmi;        // number of 
  Int_t *nkpl;        // number of 
  Int_t *nhyp;        // number of 
  Int_t *ndeut;         // number of 
  Int_t *ntrit;         // number of 
  Int_t *nalpha;          // number of 
  Int_t *nother;          // number of 
  Int_t ifinnu, ifinpi, ifinet, ifinka, ifinhy, cerele, cerhad;
  Int_t lpct1, nstep;
  Float_t thstep;
  Float_t **plong, **aplong;
  Float_t bunch;
  Float_t timefirst, timelast;
  ifstream input_file;
  Int_t opened;

protected:
  void init( Int_t n );

public:
  CORStatfile ( void );   // constructor
  CORStatfile ( Int_t n );   // overloaded constructor
  ~CORStatfile ( void );  // destructor
  // copy constructor defaults

  inline void set_bunch ( Float_t b ) { bunch = b; };
  void read ();    // reads data from input file
  void write(ofstream &output_file);  // writes data to given output file
  Int_t openfile ( char *fname ); // sets and opens new sta* file
  void closefile (); // sets and opens new sta* file
  inline Float_t get_tfirst() { return ( timefirst ); }
  inline Float_t get_tlast() { return ( timelast ); }
  
};

// @endcode

#endif // not defined CORStatfile_Class

