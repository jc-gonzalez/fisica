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

// reads EventHeader from binary input stream
Int_t COREventHeader::read ( ifstream &is ) {
    int cnt = 0;
    
    is.read((char*)((this->EVTH)), 4 * sizeof(char)); cnt += is.gcount();
    is.read((char*)(&(this->EvtNumber)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->PrimaryID)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->Etotal)), sizeof(Float_t)); cnt += is.gcount();   
    is.read((char*)(&(this->Thick0)), sizeof(Float_t)); cnt += is.gcount();   
    is.read((char*)(&(this->FirstTarget)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->zFirstInt)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)((this->p)), 3 * sizeof(Float_t)); cnt += is.gcount();     
    is.read((char*)(&(this->Theta)), sizeof(Float_t)); cnt += is.gcount(); 
    is.read((char*)(&(this->Phi)), sizeof(Float_t)); cnt += is.gcount(); 

    is.read((char*)(&(this->NumRndSeq)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->RndData[0][0])), 3 * 10 * sizeof(Float_t)); cnt += is.gcount();
  
    is.read((char*)(&(this->RunNumber)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->DateRun)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->VersionPGM)), sizeof(Float_t)); cnt += is.gcount();

    is.read((char*)(&(this->NumObsLev)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)((this->HeightLev)), 10 * sizeof(Float_t)); cnt += is.gcount(); 

    is.read((char*)(&(this->SlopeSpec)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->ELowLim)), sizeof(Float_t)); cnt += is.gcount();   
    is.read((char*)(&(this->EUppLim)), sizeof(Float_t)); cnt += is.gcount();   

    is.read((char*)(&(this->Ecutoffh)), sizeof(Float_t)); cnt += is.gcount();  
    is.read((char*)(&(this->Ecutoffm)), sizeof(Float_t)); cnt += is.gcount();  
    is.read((char*)(&(this->Ecutoffe)), sizeof(Float_t)); cnt += is.gcount();  
    is.read((char*)(&(this->Ecutoffg)), sizeof(Float_t)); cnt += is.gcount();  

    is.read((char*)(&(this->NFLAIN)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->NFLDIF)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->NFLPI0)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->NFLPIF)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->NFLCHE)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->NFRAGM)), sizeof(Float_t)); cnt += is.gcount(); 
 
    is.read((char*)(&(this->Bx)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->By)), sizeof(Float_t)); cnt += is.gcount();
  
    is.read((char*)(&(this->EGS4yn)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->NKGyn)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->GHEISHAyn)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->VENUSyn)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->CERENKOVyn)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->NEUTRINOyn)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->HORIZONTyn)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->COMPUTER)), sizeof(Float_t)); cnt += is.gcount();

    is.read((char*)(&(this->ThetaMin)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->ThetaMax)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->PhiMin)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->PhiMax)), sizeof(Float_t)); cnt += is.gcount();

    is.read((char*)(&(this->CBunchSize)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->CDetInX)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->CDetInY)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->CSpacInX)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->CSpacInY)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->CLenInX)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->CLenInY)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->COutput)), sizeof(Float_t)); cnt += is.gcount();

    is.read((char*)(&(this->AngleNorthX)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->MuonInfo)), sizeof(Float_t)); cnt += is.gcount();

    is.read((char*)(&(this->StepLength)), sizeof(Float_t)); cnt += is.gcount();
    is.read((char*)(&(this->CWaveLower)), sizeof(Float_t)); cnt += is.gcount();       
    is.read((char*)(&(this->CWaveUpper)), sizeof(Float_t)); cnt += is.gcount();       
    is.read((char*)(&(this->Multipl)), sizeof(Float_t)); cnt += is.gcount();       
    is.read((char*)(&(this->CorePos[0][0])), 2 * 20 * sizeof(Float_t)); cnt += is.gcount();   

    is.read((char*)(&(this->dmmy1)), sizeof(Float_t)); cnt += is.gcount(); 
    is.read((char*)(&(this->SpinTheta)), sizeof(Float_t)); cnt += is.gcount(); 
    is.read((char*)(&(this->SpinPhi)), sizeof(Float_t)); cnt += is.gcount();   
    is.read((char*)((this->dmmy2)), 132 * sizeof(Float_t)); cnt += is.gcount(); 

    // is.read ( (char *)this, sizeof( float ) * 273 );
    std::cerr << "EVTH: " << cnt << " bytes, " << cnt * 0.25 << " words\n";
    return cnt;
}

// writes EventHeader to binary output stream
Int_t COREventHeader::write ( ofstream &os ) {
    os.write ( (char *)this, sizeof( float ) * 273 ); 
    return 0;
}

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

