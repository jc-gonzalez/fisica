/////////////////////////////////////////////////////////////////
//
// COREvent
//
//  Created: Tue Apr 28 16:14:05 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for Event-classes
//  Notes:   
//
/////////////////////////////////////////////////////////////////

#ifndef COREvent_Class
#define COREvent_Class

#include "jctypes.h"

#include "COREventHeader.hxx"

class COREvent {

public:
  Int_t           NCphotons;
  Float_t         TimeBegin;
  Float_t         TimeEnd;
  COREventHeader  EvtHdr;
  UInt_t          Flag;

public:
  COREvent(void) {} // default constructor
  virtual ~COREvent(void) {} // default destructor

  // set number of Cphotons for this event
  inline void SetNCphotons( Int_t n ) { NCphotons = n; }

  // set flag with a number
  inline void SetFlag( UInt_t f ) { Flag = f; }

  // toggle flag bit number b
  inline void ToggleFlagBit( UInt_t b ) { }

  // get number of Cherenkov photons
  inline Int_t GetNCphotons() const { return NCphotons; }

  // get EventHeader pointer
  inline COREventHeader *GetHeader() { return &EvtHdr; }

  // Get flag value
  inline UInt_t GetFlag() const { return Flag; }

  // get status of flag bit number b
  inline UInt_t GetFlagBit( UInt_t b ) { return ( Flag && 1 << (b-1) ); }

  // set times for this event (first and last Cherenkov photons)
  void SetTimes( Float_t t1, Float_t t2 ) {
	TimeBegin = t1;
	TimeEnd = t2;
  }

  // get times of this event (first and last Cherenkov photons)
  Float_t GetTimes(Float_t *t1, Float_t *t2) const { 
	*t1 = TimeBegin;
	*t2 = TimeEnd;
	return (TimeEnd - TimeBegin); 
  }

};

#endif  // not defined COREvent_Class

