/***************************************************************************
                            -  description
                             -------------------
    begin                : Tue Jan 18 2000
    copyright            : (C) 2000 by Jose Carlos Gonzalez
    email                : gonzalez@mppmu.mpg.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef EVTHDR_H
#define EVTHDR_H

#define ITEM_LIST   /* list of items in the event header */     \
  T(EvtHdr_ntrigger),         \
  T(EvtHdr_n),                \
  T(EvtHdr_primary),          \
  T(EvtHdr_energy),           \
  T(EvtHdr_cored),            \
  T(EvtHdr_impact),           \
  T(EvtHdr_xcore),            \
  T(EvtHdr_ycore),            \
  T(EvtHdr_theta),            \
  T(EvtHdr_phi),              \
  T(EvtHdr_dangle),           \
  T(EvtHdr_dtheta),           \
  T(EvtHdr_dphi),             \
  T(EvtHdr_trigger),          \
  T(EvtHdr_ncphs),            \
  T(EvtHdr_nphes),            \
  T(EvtHdr_nphes2),           \
  T(EvtHdr_length),           \
  T(EvtHdr_width),            \
  T(EvtHdr_dist),             \
  T(EvtHdr_xdist),            \
  T(EvtHdr_azw),              \
  T(EvtHdr_miss),             \
  T(EvtHdr_alpha),            \
  T(EvtHdr_conc2),            \
  T(EvtHdr_conc3),            \
  T(EvtHdr_conc4),            \
  T(EvtHdr_conc5),            \
  T(EvtHdr_conc6),            \
  T(EvtHdr_conc7),            \
  T(EvtHdr_conc8),            \
  T(EvtHdr_conc9),            \
  T(EvtHdr_xmax),             \
  T(EvtHdr_ymax),             \
  T(EvtHdr_xm),               \
  T(EvtHdr_ym),               \
  T(EvtHdr_beta),             \
  T(EvtHdr_m2xy),             \
  T(EvtHdr_asymx),            \
  T(EvtHdr_asymy),            \
  T(EvtHdr_phiasym),          \
  T(EvtHdr_l1),               \
  T(EvtHdr_l2),               \
  T(EvtHdr_w1),               \
  T(EvtHdr_w2),               \
  T(EvtHdr_tstddev),          \
  T(EvtHdr_tmax),             \
  T(EvtHdr_t0),               \
  T(EvtHdr_t1),               \
  T(EvtHdr_trange)

#define T(x)  x             // define T() as the name as it is

enum eEvtHdr_Variables {
  ITEM_LIST,
  EvtHdr_NUMBER_OF_VARIABLES
};

#undef T

#define T(x)  #x              // define T() as the string of x

const char *const cEvtHdr_Variables[] = {
  ITEM_LIST
};

#undef T

/**********************************************************************
 *  EvtHdr class
 **********************************************************************/

class EvtHdr
{
 public:
  EvtHdr() {
    nVariables = EvtHdr_NUMBER_OF_VARIABLES;
    data = new float[ nVariables ];
  }
  
  ~EvtHdr() {
    delete [] data;
  }
  
  const float & operator[](int i) const { return *(data+i); }
  float & operator[](int i) { return *(data+i); }
  
  const float & get(int i) const { return *(data+i); }
  float & get(int i) { return *(data+i); }
  
  int size() { return nVariables; }

 private:
  float * data;
  int nVariables;
};

#endif // EVTHDR_H
 
