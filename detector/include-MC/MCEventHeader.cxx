/////////////////////////////////////////////////////////////////
//
// MCEventHeader
//
//  Created: Tue Apr 28 16:27:14 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for EventHeader-classes
//  Notes:   
//
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt MCEventHeader.cxx}

// @subsection Class {\em MCEventHeader}: Implementation

// @code

#include "MCEventHeader.hxx"

void MCEventHeader::transport( COREventHeader *e )
{
  EvtNumber      = e->EvtNumber;
  PrimaryID      = e->PrimaryID;
  Etotal         = e->Etotal;
  Thick0         = e->Thick0;
  FirstTarget    = e->FirstTarget;
  zFirstInt      = e->zFirstInt;
  Theta          = e->Theta;
  Phi            = e->Phi;
  NumRndSeq      = e->NumRndSeq;
  RunNumber      = e->RunNumber;
  DateRun        = e->DateRun;
  impact_union.VersionPGM     = e->VersionPGM;
  NumObsLev      = e->NumObsLev;
  SlopeSpec      = e->SlopeSpec;
  ELowLim        = e->ELowLim;
  EUppLim        = e->EUppLim;
  ThetaMin       = e->ThetaMin;
  ThetaMax       = e->ThetaMax;
  PhiMin         = e->PhiMin;
  PhiMax         = e->PhiMax;
  CWaveLower     = e->CWaveLower;
  CWaveUpper     = e->CWaveUpper;

  memcpy( p, e->p, 3*sizeof(Float_t) );
  memcpy( RndData, e->RndData, 30*sizeof(Float_t) );
  memcpy( HeightLev, e->HeightLev, 10*sizeof(Float_t) );
  memcpy( CorePos, e->CorePos, 40*sizeof(Float_t) );
}

// @endcode
