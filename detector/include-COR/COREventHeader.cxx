/////////////////////////////////////////////////////////////////
//
// COREventHeader
//
//  Created: Tue Apr 28 16:27:14 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for EventHeader-classes
//  Notes:   
//
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt COREventHeader.cxx}

// @subsection Class {\em COREventHeader}: Implementation

// @code

#include "COREventHeader.hxx"

void 
COREventHeader::print(void) {

  cout << "  EVTH           :" << (char*)EVTH << endl;
  cout << "  EvtNumber      :" << EvtNumber << endl;
  cout << "  PrimaryID      :" << PrimaryID << endl;
  cout << "  Etotal         :" << Etotal    << endl;
  cout << "  Thick0         :" << Thick0    << endl;
  cout << "  FirstTarget    :" << FirstTarget << endl;
  cout << "  zFirstInt      :" << zFirstInt << endl;
  cout << "  p[3]           :" << p[0] << ' ' << p[1] << ' ' << p[2] << endl;
  cout << "  Theta          :" << Theta  << endl;
  cout << "  Phi            :" << Phi  << endl;
                                
  cout << "  NumRndSeq      :" << NumRndSeq << endl;
                                
  cout << "  RunNumber      :" << RunNumber << endl;
  cout << "  DateRun        :" << DateRun << endl;
  cout << "  VersionPGM     :" << VersionPGM << endl;
                                
  cout << "  NumObsLev      :" << NumObsLev << endl;
  cout << "  HeightLev[0]   :" << HeightLev[0]  << endl;
                                
  cout << "  SlopeSpec      :" << SlopeSpec << endl;
  cout << "  ELowLim        :" << ELowLim    << endl;
  cout << "  EUppLim        :" << EUppLim    << endl;
                                
  cout << "  Ecutoffh       :" << Ecutoffh   << endl;
  cout << "  Ecutoffm       :" << Ecutoffm   << endl;
  cout << "  Ecutoffe       :" << Ecutoffe   << endl;
  cout << "  Ecutoffg       :" << Ecutoffg   << endl;
                                
  cout << "  NFLAIN         :" << NFLAIN << endl;
  cout << "  NFLDIF         :" << NFLDIF << endl;
  cout << "  NFLPI0         :" << NFLPI0 << endl;
  cout << "  NFLPIF         :" << NFLPIF << endl;
  cout << "  NFLCHE         :" << NFLCHE << endl;
  cout << "  NFRAGM         :" << NFRAGM  << endl;
                                
  cout << "  Bx             :" << Bx << endl;
  cout << "  By             :" << By << endl;
                                
  cout << "  EGS4yn         :" << EGS4yn << endl;
  cout << "  NKGyn          :" << NKGyn << endl;
  cout << "  GHEISHAyn      :" << GHEISHAyn << endl;
  cout << "  VENUSyn        :" << VENUSyn << endl;
  cout << "  CERENKOVyn     :" << CERENKOVyn << endl;
  cout << "  NEUTRINOyn     :" << NEUTRINOyn << endl;
  cout << "  HORIZONTyn     :" << HORIZONTyn << endl;
  cout << "  COMPUTER       :" << COMPUTER << endl;
                                
  cout << "  ThetaMin       :" << ThetaMin << endl;
  cout << "  ThetaMax       :" << ThetaMax << endl;
  cout << "  PhiMin         :" << PhiMin << endl;
  cout << "  PhiMax         :" << PhiMax << endl;
                                
  cout << "  CBunchSize     :" << CBunchSize << endl;
  cout << "  CDetInX        :" << CDetInX << endl;
  cout << "  CDetInY        :" << CDetInY << endl;
  cout << "  CSpacInX       :" << CSpacInX << endl;
  cout << "  CSpacInY       :" << CSpacInY << endl;
  cout << "  CLenInX        :" << CLenInX << endl;
  cout << "  CLenInY        :" << CLenInY << endl;
  cout << "  COutput        :" << COutput << endl;
                                
  cout << "  AngleNorthX    :" << AngleNorthX << endl;
  cout << "  MuonInfo       :" << MuonInfo << endl;
                                
  cout << "  StepLength     :" << StepLength << endl;
  cout << "  CWaveLower     :" << CWaveLower        << endl;
  cout << "  CWaveUpper     :" << CWaveUpper        << endl;
  cout << "  Multipl        :" << Multipl        << endl;
  cout << "  CorePos{1}     :" 
       << '(' << CorePos[0][0] 
       << ',' << CorePos[1][0] << ") m" << endl;
                                
  cout << "  SpinTheta      :" << SpinTheta  << endl;
  cout << "  SpinPhi        :" << SpinPhi    << endl;

  cout << flush;
}

// @endcode << endl;

