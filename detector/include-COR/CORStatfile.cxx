/////////////////////////////////////////////////////////////////
//
// CORStatfile
//
//  Created: Tue Apr 28 16:14:05 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for Event-classes
//  Notes:   
//
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt CORStatfile.cxx}

// @subsection Class {\em CORStatfile}: Implementation

// @code

#include "CORStatfile.hxx"

CORStatfile::CORStatfile () 
{
  init ( 79 );  // assumes thstep = 10 g/cm2    (790/10 = 79)
}

CORStatfile::CORStatfile ( Int_t n )
{
  init ( n );
}

void
CORStatfile::init ( Int_t n ) 
{
  Int_t i, k;

  nstep = n;

  evth   = new Float_t[MAXBUF]; 	  // event header
  evte   = new Float_t[MAXBUF]; 	  // event end
  
  nproto = new Int_t[nstep];
  nprotb = new Int_t[nstep];
  nneutr = new Int_t[nstep];
  nneutb = new Int_t[nstep];
  nphoto = new Int_t[nstep];
  nelect = new Int_t[nstep];
  nposit = new Int_t[nstep];
  nnu    = new Int_t[nstep];
  nmum   = new Int_t[nstep];
  nmup   = new Int_t[nstep];
  npi0   = new Int_t[nstep];
  npim   = new Int_t[nstep];
  npip   = new Int_t[nstep];
  nk0l   = new Int_t[nstep];
  nk0s   = new Int_t[nstep];
  nkmi   = new Int_t[nstep];
  nkpl   = new Int_t[nstep];
  nhyp   = new Int_t[nstep];
  ndeut  = new Int_t[nstep];
  ntrit  = new Int_t[nstep];
  nalpha = new Int_t[nstep];
  nother = new Int_t[nstep];

  plong = new Float_t * [9];
  aplong = new Float_t * [9];

  for ( i=0; i<9; i++ ) {
	plong[i] = new Float_t[NPLONG];
	aplong[i] = new Float_t[NPLONG];
  }
	
  for ( i=0; i<9; i++ )
	for ( k=0; k<NPLONG; k++ )
	  plong[i][k] = aplong[i][k] = 0.;

  opened = 0;
  bunch = 1.0;
}

CORStatfile::~CORStatfile () 
{
  Int_t i;

  for ( i=0; i<9; i++ ) {
	delete [] plong[i];
	delete [] aplong[i];
	plong[i] = aplong[i] = NULL;
  }

  delete [] plong;
  delete [] aplong;
  plong = aplong = NULL;

  delete [] evth;
  delete [] evte;
  evth = evte = NULL;
  
  delete [] nproto;
  delete [] nprotb;
  delete [] nneutr;
  delete [] nneutb;
  delete [] nphoto;
  delete [] nelect;
  delete [] nposit;
  delete [] nnu;
  delete [] nmum;
  delete [] nmup;
  delete [] npi0;
  delete [] npim;
  delete [] npip;
  delete [] nk0l;
  delete [] nk0s;
  delete [] nkmi;
  delete [] nkpl;
  delete [] nhyp;
  delete [] ndeut;
  delete [] ntrit;
  delete [] nalpha;
  delete [] nother;

  nproto = nprotb = nneutr = nneutb = 
	nphoto = nelect = nposit = nnu =   
	nmum = nmup = npi0 = npim = npip = 
	nk0l = nk0s = nkmi = nkpl = nhyp = 
	ndeut = ntrit = nalpha = nother = NULL;

}

Int_t 
CORStatfile::openfile ( char *fname )
{
  if ( input_file != NULL )
	input_file.close();
  input_file.open( fname );
  opened = 1;
  return ( ! input_file.bad() );
}

void
CORStatfile::closefile ( void )
{
  input_file.close();
  opened = 0;
}

void
CORStatfile::read()
{
  Int_t i, k;
  Float_t f;

  input_file.read ( (char *)evth, sizeof (Float_t) * MAXBUF );
  input_file.read ( (char *)evte, sizeof (Float_t) * MAXBUF );
  input_file.read ( (char *)&timefirst, sizeof (Float_t) );
  input_file.read ( (char *)&timelast, sizeof (Float_t) );

  for (i = 0; i < 10; i++) {

      input_file.read ( (char *)(nproto + i), sizeof (Int_t) );
      input_file.read ( (char *)(nprotb + i), sizeof (Int_t) );
      input_file.read ( (char *)(nneutr + i), sizeof (Int_t) );
      input_file.read ( (char *)(nneutb + i), sizeof (Int_t) );
      input_file.read ( (char *)(nphoto + i), sizeof (Int_t) );
      input_file.read ( (char *)(nelect + i), sizeof (Int_t) );
      input_file.read ( (char *)(nposit + i), sizeof (Int_t) );
      input_file.read ( (char *)(nnu + i), sizeof (Int_t) );
      input_file.read ( (char *)(nmum + i), sizeof (Int_t) );
      input_file.read ( (char *)(nmup + i), sizeof (Int_t) );
      input_file.read ( (char *)(npi0 + i), sizeof (Int_t) );
      input_file.read ( (char *)(npim + i), sizeof (Int_t) );
      input_file.read ( (char *)(npip + i), sizeof (Int_t) );
      input_file.read ( (char *)(nk0l + i), sizeof (Int_t) );
      input_file.read ( (char *)(nk0s + i), sizeof (Int_t) );
      input_file.read ( (char *)(nkmi + i), sizeof (Int_t) );
      input_file.read ( (char *)(nkpl + i), sizeof (Int_t) );
      input_file.read ( (char *)(nhyp + i), sizeof (Int_t) );
      input_file.read ( (char *)(ndeut + i), sizeof (Int_t) );
      input_file.read ( (char *)(ntrit + i), sizeof (Int_t) );
      input_file.read ( (char *)(nalpha + i), sizeof (Int_t) );
      input_file.read ( (char *)(nother + i), sizeof (Int_t) );

  }

  input_file.read ( (char *)&ifinnu, sizeof (Int_t) );
  input_file.read ( (char *)&ifinpi, sizeof (Int_t) );
  input_file.read ( (char *)&ifinet, sizeof (Int_t) );
  input_file.read ( (char *)&ifinka, sizeof (Int_t) );
  input_file.read ( (char *)&ifinhy, sizeof (Int_t) );
  input_file.read ( (char *)&cerele, sizeof (Int_t) );
  input_file.read ( (char *)&cerhad, sizeof (Int_t) );

  input_file.read ( (char *)&lpct1, sizeof (Int_t) );
  input_file.read ( (char *)&nstep, sizeof (Int_t) );
  input_file.read ( (char *)&thstep, sizeof (Float_t) );

  for (k = 0; k < 9; k++)
	for (i = 0; i < nstep; i++) {
	  input_file.read ( (char *)&f, sizeof (Float_t) );
	  plong[k][i] = f;
	}

  for (k = 0; k < 9; k++)
	for (i = 0; i < nstep; i++)
	  aplong[k][i] += plong[k][i] * bunch;
  
}

void
CORStatfile::write(ofstream &output_file)
{
  Int_t i, k;
  Float_t f = 0.;

  output_file.write ( (char *)evth, sizeof (Float_t) * MAXBUF );
  output_file.write ( (char *)evte, sizeof (Float_t) * MAXBUF );
  output_file.write ( (char *)&timefirst, sizeof (Float_t) );
  output_file.write ( (char *)&timelast, sizeof (Float_t) );

  for (i = 0; i < 10; i++) {

      output_file.write ( (char *)(nproto + i), sizeof (Int_t) );
      output_file.write ( (char *)(nprotb + i), sizeof (Int_t) );
      output_file.write ( (char *)(nneutr + i), sizeof (Int_t) );
      output_file.write ( (char *)(nneutb + i), sizeof (Int_t) );
      output_file.write ( (char *)(nphoto + i), sizeof (Int_t) );
      output_file.write ( (char *)(nelect + i), sizeof (Int_t) );
      output_file.write ( (char *)(nposit + i), sizeof (Int_t) );
      output_file.write ( (char *)(nnu + i), sizeof (Int_t) );
      output_file.write ( (char *)(nmum + i), sizeof (Int_t) );
      output_file.write ( (char *)(nmup + i), sizeof (Int_t) );
      output_file.write ( (char *)(npi0 + i), sizeof (Int_t) );
      output_file.write ( (char *)(npim + i), sizeof (Int_t) );
      output_file.write ( (char *)(npip + i), sizeof (Int_t) );
      output_file.write ( (char *)(nk0l + i), sizeof (Int_t) );
      output_file.write ( (char *)(nk0s + i), sizeof (Int_t) );
      output_file.write ( (char *)(nkmi + i), sizeof (Int_t) );
      output_file.write ( (char *)(nkpl + i), sizeof (Int_t) );
      output_file.write ( (char *)(nhyp + i), sizeof (Int_t) );
      output_file.write ( (char *)(ndeut + i), sizeof (Int_t) );
      output_file.write ( (char *)(ntrit + i), sizeof (Int_t) );
      output_file.write ( (char *)(nalpha + i), sizeof (Int_t) );
      output_file.write ( (char *)(nother + i), sizeof (Int_t) );

  }

  output_file.write ( (char *)&ifinnu, sizeof (Int_t) );
  output_file.write ( (char *)&ifinpi, sizeof (Int_t) );
  output_file.write ( (char *)&ifinet, sizeof (Int_t) );
  output_file.write ( (char *)&ifinka, sizeof (Int_t) );
  output_file.write ( (char *)&ifinhy, sizeof (Int_t) );
  output_file.write ( (char *)&cerele, sizeof (Int_t) );
  output_file.write ( (char *)&cerhad, sizeof (Int_t) );

  output_file.write ( (char *)&lpct1, sizeof (Int_t) );
  output_file.write ( (char *)&nstep, sizeof (Int_t) );
  output_file.write ( (char *)&thstep, sizeof (Float_t) );

  for (k = 0; k < 9; k++)
    for (i = 0; i < nstep; i++) {
      output_file.write ( (char *)&f, sizeof (Float_t) );
    }
  
}

// @endcode
