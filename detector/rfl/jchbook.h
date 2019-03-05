/**********************************************************************
 **********************************************************************
 * jchbook
 *
 *   Created: Tue Sep 15 11:59:38 MET DST 1998
 *   Author.: Jose Carlos Gonzalez
 *   Notes..: Porting HBOOK to C++ using package cppf77 headers
 *            from Carsten Arnholm. Thanks, Carsten.
 *
 **********************************************************************
 **********************************************************************/

// @T \newpage

// @section Source code of {\tt jchbook.h}

// @code

/* Begin */

#ifndef HBOOK_H_F77_STUB
#define HBOOK_H_F77_STUB

#define F77_STUB_REQUIRED
#include <fortran.h>

SUBROUTINE_F77 harray_(int*,int*,int* );
SUBROUTINE HARRAY(INTEGER* A1,INTEGER* A2,INTEGER* A3 )
{ harray_( A1, A2, A3 ); }

SUBROUTINE_F77 hbandx_(int*,float*,float*,float* );
SUBROUTINE HBANDX(INTEGER* A1,REAL* A2,REAL* A3,REAL* A4 )
{ hbandx_( A1, A2, A3, A4 ); }

SUBROUTINE_F77 hbandy_(int*,float*,float*,float* );
SUBROUTINE HBANDY(INTEGER* A1,REAL* A2,REAL* A3,REAL* A4 )
{ hbandy_( A1, A2, A3, A4 ); }

SUBROUTINE_F77 hbarx_(int* );
SUBROUTINE HBARX(INTEGER* A1 )
{ hbarx_( A1 ); }

SUBROUTINE_F77 hbary_(int* );
SUBROUTINE HBARY(INTEGER* A1 )
{ hbary_( A1 ); }

SUBROUTINE_F77 hbfun1_(int*,char*,int*,float*,float*,float*,int );
SUBROUTINE HBFUN1(INTEGER* A1,CHARACTER A2,INTEGER* A3,REAL* A4,REAL* A5,REAL* A6 )
{ hbfun1_( A1, A2.rep, A3, A4, A5, A6, A2.len ); }

SUBROUTINE_F77 hbfun2_(int*,char*,int*,float*,float*,int*,float*,float*,float*,int );
SUBROUTINE HBFUN2(INTEGER* A1,CHARACTER A2,INTEGER* A3,REAL* A4,REAL* A5,INTEGER* A6,REAL* A7,REAL* A8,REAL* A9 )
{ hbfun2_( A1, A2.rep, A3, A4, A5, A6, A7, A8, A9, A2.len ); }

SUBROUTINE_F77 hbigbi_(int*,int* );
SUBROUTINE HBIGBI(INTEGER* A1,INTEGER* A2 )
{ hbigbi_( A1, A2 ); }

SUBROUTINE_F77 hbinsz_(char*,int );
SUBROUTINE HBINSZ(CHARACTER A1 )
{ hbinsz_( A1.rep, A1.len ); }

SUBROUTINE_F77 hbnamc_(int*,char*,char*,char*,int,int,int );
SUBROUTINE HBNAMC(INTEGER* A1,CHARACTER A2,CHARACTER A3,CHARACTER A4 )
{ hbnamc_( A1, A2.rep, A3.rep, A4.rep, A2.len, A3.len, A4.len ); }

SUBROUTINE_F77 hbname_(int*,char*,int*,char*,int,int );
SUBROUTINE HBNAME(INTEGER* A1,CHARACTER A2,INTEGER* A3,CHARACTER A4 )
{ hbname_( A1, A2.rep, A3, A4.rep, A2.len, A4.len ); }

SUBROUTINE_F77 hbnt_(int*,char*,char*,int,int );
SUBROUTINE HBNT(INTEGER* A1,CHARACTER A2,CHARACTER A3 )
{ hbnt_( A1, A2.rep, A3.rep, A2.len, A3.len ); }

SUBROUTINE_F77 hbookb_(int*,char*,int*,float*,float*,int );
SUBROUTINE HBOOKB(INTEGER* A1,CHARACTER A2,INTEGER* A3,REAL* A4,REAL* A5 )
{ hbookb_( A1, A2.rep, A3, A4, A5, A2.len ); }

SUBROUTINE_F77 hbookn_(int*,char*,int*,char*,int*,char*,int,int,int );
SUBROUTINE HBOOKN(INTEGER* A1,CHARACTER A2,INTEGER* A3,CHARACTER A4,INTEGER* A5,CHARACTER A6 )
{ hbookn_( A1, A2.rep, A3, A4.rep, A5, A6.rep, A2.len, A4.len, A6.len ); }

SUBROUTINE_F77 hbook1_(int*,char*,int*,float*,float*,float*,int );
SUBROUTINE HBOOK1(INTEGER* A1,CHARACTER A2,INTEGER* A3,REAL* A4,REAL* A5,REAL* A6 )
{ hbook1_( A1, A2.rep, A3, A4, A5, A6, A2.len ); }

SUBROUTINE_F77 hbook2_(int*,char*,int*,float*,float*,int*,float*,float*,float*,int );
SUBROUTINE HBOOK2(INTEGER* A1,CHARACTER A2,INTEGER* A3,REAL* A4,REAL* A5,INTEGER* A6,REAL* A7,REAL* A8,REAL* A9 )
{ hbook2_( A1, A2.rep, A3, A4, A5, A6, A7, A8, A9, A2.len ); }

SUBROUTINE_F77 hbpro_(int*,float* );
SUBROUTINE HBPRO(INTEGER* A1,REAL* A2 )
{ hbpro_( A1, A2 ); }

SUBROUTINE_F77 hbprof_(int*,char*,int*,float*,float*,float*,float*,char*,int,int );
SUBROUTINE HBPROF(INTEGER* A1,CHARACTER A2,INTEGER* A3,REAL* A4,REAL* A5,REAL* A6,REAL* A7,CHARACTER A8 )
{ hbprof_( A1, A2.rep, A3, A4, A5, A6, A7, A8.rep, A2.len, A8.len ); }

SUBROUTINE_F77 hbprox_(int*,float* );
SUBROUTINE HBPROX(INTEGER* A1,REAL* A2 )
{ hbprox_( A1, A2 ); }

SUBROUTINE_F77 hbproy_(int*,float* );
SUBROUTINE HBPROY(INTEGER* A1,REAL* A2 )
{ hbproy_( A1, A2 ); }

SUBROUTINE_F77 hbset_(char*,int*,int*,int );
SUBROUTINE HBSET(CHARACTER A1,INTEGER* A2,INTEGER* A3 )
{ hbset_( A1.rep, A2, A3, A1.len ); }

SUBROUTINE_F77 hbslix_(int*,int*,float* );
SUBROUTINE HBSLIX(INTEGER* A1,INTEGER* A2,REAL* A3 )
{ hbslix_( A1, A2, A3 ); }

SUBROUTINE_F77 hbsliy_(int*,int*,float* );
SUBROUTINE HBSLIY(INTEGER* A1,INTEGER* A2,REAL* A3 )
{ hbsliy_( A1, A2, A3 ); }

SUBROUTINE_F77 hcdir_(char*,char*,int,int );
SUBROUTINE HCDIR(CHARACTER A1,CHARACTER A2 )
{ hcdir_( A1.rep, A2.rep, A1.len, A2.len ); }

SUBROUTINE_F77 hcompa_(int*,int* );
SUBROUTINE HCOMPA(INTEGER* A1,INTEGER* A2 )
{ hcompa_( A1, A2 ); }

SUBROUTINE_F77 hcopy_(int*,int*,char*,int );
SUBROUTINE HCOPY(INTEGER* A1,INTEGER* A2,CHARACTER A3 )
{ hcopy_( A1, A2, A3.rep, A3.len ); }

SUBROUTINE_F77 hcopym_(int*,int*,int* );
SUBROUTINE HCOPYM(INTEGER* A1,INTEGER* A2,INTEGER* A3 )
{ hcopym_( A1, A2, A3 ); }

SUBROUTINE_F77 hdelet_(int* );
SUBROUTINE HDELET(INTEGER* A1 )
{ hdelet_( A1 ); }

SUBROUTINE_F77 hderiv_(float* );
SUBROUTINE HDERI(REAL* A1 )
{ hderiv_( A1 ); }

SUBROUTINE_F77 hdiff_(int*,int*,float*,char*,int );
SUBROUTINE HDIFF(INTEGER* A1,INTEGER* A2,REAL* A3,CHARACTER A4 )
{ hdiff_( A1, A2, A3, A4.rep, A4.len ); }

SUBROUTINE_F77 hdiffb_(int*,int*,float*,int*,char*,int*,float*,int );
SUBROUTINE HDIFFB(INTEGER* A1,INTEGER* A2,REAL* A3,INTEGER* A4,CHARACTER A5,INTEGER* A6,REAL* A7 )
{ hdiffb_( A1, A2, A3, A4, A5.rep, A6, A7, A5.len ); }

SUBROUTINE_F77 hdump_(int* );
SUBROUTINE HDUMP(INTEGER* A1 )
{ hdump_( A1 ); }

SUBROUTINE_F77 hermes_(int* );
SUBROUTINE HERMES(INTEGER* A1 )
{ hermes_( A1 ); }

SUBROUTINE_F77 hfc2_(int*,int*,char*,int*,char*,float*,char*,int,int,int );
SUBROUTINE HFC2(INTEGER* A1,INTEGER* A2,CHARACTER A3,INTEGER* A4,CHARACTER A5,REAL* A6,CHARACTER A7 )
{ hfc2_( A1, A2, A3.rep, A4, A5.rep, A6, A7.rep, A3.len, A5.len, A7.len ); }

SUBROUTINE_F77 hff1_(int*,int*,float*,float* );
SUBROUTINE HFF1(INTEGER* A1,INTEGER* A2,REAL* A3,REAL* A4 )
{ hff1_( A1, A2, A3, A4 ); }

SUBROUTINE_F77 hff2_(int*,int*,float*,float*,float* );
SUBROUTINE HFF2(INTEGER* A1,INTEGER* A2,REAL* A3,REAL* A4,REAL* A5 )
{ hff2_( A1, A2, A3, A4, A5 ); }

SUBROUTINE_F77 hfill_(int*,float*,float*,float* );
SUBROUTINE HFILL(INTEGER* A1,REAL* A2,REAL* A3,REAL* A4 )
{ hfill_( A1, A2, A3, A4 ); }

SUBROUTINE_F77 hfinam_(int*,char*,int*,int );
SUBROUTINE HFINAM(INTEGER* A1,CHARACTER A2,INTEGER* A3 )
{ hfinam_( A1, A2.rep, A3, A2.len ); }

SUBROUTINE_F77 hfitex_(int*,float*,float*,float*,int*,float* );
SUBROUTINE HFITEX(INTEGER* A1,REAL* A2,REAL* A3,REAL* A4,INTEGER* A5,REAL* A6 )
{ hfitex_( A1, A2, A3, A4, A5, A6 ); }

SUBROUTINE_F77 hfitga_(int*,float*,float*,float*,float*,int*,float* );
SUBROUTINE HFITGA(INTEGER* A1,REAL* A2,REAL* A3,REAL* A4,REAL* A5,INTEGER* A6,REAL* A7 )
{ hfitga_( A1, A2, A3, A4, A5, A6, A7 ); }

SUBROUTINE_F77 hfith_(int*,float*,char*,int*,float*,float*,float*,float*,float*,float*,int );
SUBROUTINE HFITH(INTEGER* A1,REAL* A2,CHARACTER A3,INTEGER* A4,REAL* A5,REAL* A6,REAL* A7,REAL* A8,REAL* A9,REAL* A10 )
{ hfith_( A1, A2, A3.rep, A4, A5, A6, A7, A8, A9, A10, A3.len ); }

SUBROUTINE_F77 hfithn_(int*,char*,char*,int*,float*,float*,float*,float*,float*,float*,int,int );
SUBROUTINE HFITHN(INTEGER* A1,CHARACTER A2,CHARACTER A3,INTEGER* A4,REAL* A5,REAL* A6,REAL* A7,REAL* A8,REAL* A9,REAL* A10 )
{ hfithn_( A1, A2.rep, A3.rep, A4, A5, A6, A7, A8, A9, A10, A2.len, A3.len ); }

SUBROUTINE_F77 hfitl_(int*,float*,int*,double*,float*,int*,float*,float*,float*,float*,float* );
SUBROUTINE HFITL(INTEGER* A1,REAL* A2,INTEGER* A3,DOUBLE_PRECISION* A4,REAL* A5,INTEGER* A6,REAL* A7,REAL* A8,REAL* A9,REAL* A10,REAL* A11 )
{ hfitl_( A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11 ); }

SUBROUTINE_F77 hfitn_(float*,float*,float*,int*,int*,int*,float*,int*,double*,float*,int*,float*,float*,float*,float*,float* );
SUBROUTINE HFITN(REAL* A1,REAL* A2,REAL* A3,INTEGER* A4,INTEGER* A5,INTEGER* A6,REAL* A7,INTEGER* A8,DOUBLE_PRECISION* A9,REAL* A10,INTEGER* A11,REAL* A12,REAL* A13,REAL* A14,REAL* A15,REAL* A16 )
{ hfitn_( A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16 ); }

SUBROUTINE_F77 hfitpo_(int*,int*,float*,float*,int*,float* );
SUBROUTINE HFITPO(INTEGER* A1,INTEGER* A2,REAL* A3,REAL* A4,INTEGER* A5,REAL* A6 )
{ hfitpo_( A1, A2, A3, A4, A5, A6 ); }

SUBROUTINE_F77 hfits_(int*,float*,int*,double*,float*,int*,float* );
SUBROUTINE HFITS(INTEGER* A1,REAL* A2,INTEGER* A3,DOUBLE_PRECISION* A4,REAL* A5,INTEGER* A6,REAL* A7 )
{ hfits_( A1, A2, A3, A4, A5, A6, A7 ); }

SUBROUTINE_F77 hfitv_(int*,int*,int*,float*,float*,float*,float*,char*,int*,float*,float*,float*,float*,float*,float*,int );
SUBROUTINE HFIT(INTEGER* A1,INTEGER* A2,INTEGER* A3,REAL* A4,REAL* A5,REAL* A6,REAL* A7,CHARACTER A8,INTEGER* A9,REAL* A10,REAL* A11,REAL* A12,REAL* A13,REAL* A14,REAL* A15 )
{ hfitv_( A1, A2, A3, A4, A5, A6, A7, A8.rep, A9, A10, A11, A12, A13, A14, A15, A8.len ); }

SUBROUTINE_F77 hfit1_(float*,float*,float*,int*,float*,int*,double*,float*,int*,float* );
SUBROUTINE HFIT1(REAL* A1,REAL* A2,REAL* A3,INTEGER* A4,REAL* A5,INTEGER* A6,DOUBLE_PRECISION* A7,REAL* A8,INTEGER* A9,REAL* A10 )
{ hfit1_( A1, A2, A3, A4, A5, A6, A7, A8, A9, A10 ); }

SUBROUTINE_F77 hfn_(int*,float* );
SUBROUTINE HFN(INTEGER* A1,REAL* A2 )
{ hfn_( A1, A2 ); }

SUBROUTINE_F77 hfnt_(int* );
SUBROUTINE HFNT(INTEGER* A1 )
{ hfnt_( A1 ); }

SUBROUTINE_F77 hfntb_(int*,char*,int );
SUBROUTINE HFNTB(INTEGER* A1,CHARACTER A2 )
{ hfntb_( A1, A2.rep, A2.len ); }

SUBROUTINE_F77 hfpak1_(int*,int*,float*,int* );
SUBROUTINE HFPAK1(INTEGER* A1,INTEGER* A2,REAL* A3,INTEGER* A4 )
{ hfpak1_( A1, A2, A3, A4 ); }

SUBROUTINE_F77 hfunc_(int*,float* );
SUBROUTINE HFUNC(INTEGER* A1,REAL* A2 )
{ hfunc_( A1, A2 ); }

SUBROUTINE_F77 hf1_(int*,float*,float* );
SUBROUTINE HF1(INTEGER* A1,REAL* A2,REAL* A3 )
{ hf1_( A1, A2, A3 ); }

SUBROUTINE_F77 hf2_(int*,float*,float*,float* );
SUBROUTINE HF2(INTEGER* A1,REAL* A2,REAL* A3,REAL* A4 )
{ hf2_( A1, A2, A3, A4 ); }

SUBROUTINE_F77 hgfit_(int*,int*,int*,float*,float*,float*,char*,int );
SUBROUTINE HGFIT(INTEGER* A1,INTEGER* A2,INTEGER* A3,REAL* A4,REAL* A5,REAL* A6,CHARACTER A7 )
{ hgfit_( A1, A2, A3, A4, A5, A6, A7.rep, A7.len ); }

SUBROUTINE_F77 hgive_(int*,char*,int*,float*,float*,int*,float*,float*,int*,int*,int );
SUBROUTINE HGIE(INTEGER* A1,CHARACTER A2,INTEGER* A3,REAL* A4,REAL* A5,INTEGER* A6,REAL* A7,REAL* A8,INTEGER* A9,INTEGER* A10 )
{ hgive_( A1, A2.rep, A3, A4, A5, A6, A7, A8, A9, A10, A2.len ); }

SUBROUTINE_F77 hgiven_(int*,char*,int*,char*,float*,float*,int,int );
SUBROUTINE HGIEN(INTEGER* A1,CHARACTER A2,INTEGER* A3,CHARACTER A4,REAL* A5,REAL* A6 )
{ hgiven_( A1, A2.rep, A3, A4.rep, A5, A6, A2.len, A4.len ); }

SUBROUTINE_F77 hgn_(int*,int*,int*,float*,int* );
SUBROUTINE HGN(INTEGER* A1,INTEGER* A2,INTEGER* A3,REAL* A4,INTEGER* A5 )
{ hgn_( A1, A2, A3, A4, A5 ); }

SUBROUTINE_F77 hgnf_(int*,int*,float*,int* );
SUBROUTINE HGNF(INTEGER* A1,INTEGER* A2,REAL* A3,INTEGER* A4 )
{ hgnf_( A1, A2, A3, A4 ); }

SUBROUTINE_F77 hgnpar_(int*,char*,int );
SUBROUTINE HGNPAR(INTEGER* A1,CHARACTER A2 )
{ hgnpar_( A1, A2.rep, A2.len ); }

SUBROUTINE_F77 hgnt_(int*,int*,int* );
SUBROUTINE HGNT(INTEGER* A1,INTEGER* A2,INTEGER* A3 )
{ hgnt_( A1, A2, A3 ); }

SUBROUTINE_F77 hgntb_(int*,char*,int*,int*,int );
SUBROUTINE HGNTB(INTEGER* A1,CHARACTER A2,INTEGER* A3,INTEGER* A4 )
{ hgntb_( A1, A2.rep, A3, A4, A2.len ); }

SUBROUTINE_F77 hgntf_(int*,int*,int* );
SUBROUTINE HGNTF(INTEGER* A1,INTEGER* A2,INTEGER* A3 )
{ hgntf_( A1, A2, A3 ); }

SUBROUTINE_F77 hgntv_(int*,char*,int*,int*,int*,int );
SUBROUTINE HGNT(INTEGER* A1,CHARACTER A2,INTEGER* A3,INTEGER* A4,INTEGER* A5 )
{ hgntv_( A1, A2.rep, A3, A4, A5, A2.len ); }

SUBROUTINE_F77 hidall_(int*,int* );
SUBROUTINE HIDALL(INTEGER* A1,INTEGER* A2 )
{ hidall_( A1, A2 ); }

SUBROUTINE_F77 hidopt_(int*,char*,int );
SUBROUTINE HIDOPT(INTEGER* A1,CHARACTER A2 )
{ hidopt_( A1, A2.rep, A2.len ); }

SUBROUTINE_F77 hid1_(int*,int* );
SUBROUTINE HID1(INTEGER* A1,INTEGER* A2 )
{ hid1_( A1, A2 ); }

SUBROUTINE_F77 hid2_(int*,int* );
SUBROUTINE HID2(INTEGER* A1,INTEGER* A2 )
{ hid2_( A1, A2 ); }

SUBROUTINE_F77 hijxy_(int*,int*,int*,float*,float* );
SUBROUTINE HIJXY(INTEGER* A1,INTEGER* A2,INTEGER* A3,REAL* A4,REAL* A5 )
{ hijxy_( A1, A2, A3, A4, A5 ); }

SUBROUTINE_F77 hindex_();
SUBROUTINE HINDEX()
{ hindex_(); }

SUBROUTINE_F77 hipak1_(int*,int*,int*,int* );
SUBROUTINE HIPAK1(INTEGER* A1,INTEGER* A2,INTEGER* A3,INTEGER* A4 )
{ hipak1_( A1, A2, A3, A4 ); }

SUBROUTINE_F77 histdo_();
SUBROUTINE HISTDO()
{ histdo_(); }

SUBROUTINE_F77 hix_(int*,int*,float* );
SUBROUTINE HIX(INTEGER* A1,INTEGER* A2,REAL* A3 )
{ hix_( A1, A2, A3 ); }

SUBROUTINE_F77 hlabel_(int*,int*,char*,char*,int,int );
SUBROUTINE HLABEL(INTEGER* A1,INTEGER* A2,CHARACTER A3,CHARACTER A4 )
{ hlabel_( A1, A2, A3.rep, A4.rep, A3.len, A4.len ); }

SUBROUTINE_F77 hldir_(char*,char*,int,int );
SUBROUTINE HLDIR(CHARACTER A1,CHARACTER A2 )
{ hldir_( A1.rep, A2.rep, A1.len, A2.len ); }

SUBROUTINE_F77 hlimap_(int*,char*,int );
SUBROUTINE HLIMAP(INTEGER* A1,CHARACTER A2 )
{ hlimap_( A1, A2.rep, A2.len ); }

SUBROUTINE_F77 hlimit_(int* );
SUBROUTINE HLIMIT(INTEGER* A1 )
{ hlimit_( A1 ); }

SUBROUTINE_F77 hlocat_(int*,int* );
SUBROUTINE HLOCAT(INTEGER* A1,INTEGER* A2 )
{ hlocat_( A1, A2 ); }

SUBROUTINE_F77 hmaxim_(int*,float* );
SUBROUTINE HMAXIM(INTEGER* A1,REAL* A2 )
{ hmaxim_( A1, A2 ); }

SUBROUTINE_F77 hmdir_(char*,char*,int,int );
SUBROUTINE HMDIR(CHARACTER A1,CHARACTER A2 )
{ hmdir_( A1.rep, A2.rep, A1.len, A2.len ); }

SUBROUTINE_F77 hminim_(int*,float* );
SUBROUTINE HMINIM(INTEGER* A1,REAL* A2 )
{ hminim_( A1, A2 ); }

SUBROUTINE_F77 hnoent_(int*,int* );
SUBROUTINE HNOENT(INTEGER* A1,INTEGER* A2 )
{ hnoent_( A1, A2 ); }

SUBROUTINE_F77 hnorma_(int*,float* );
SUBROUTINE HNORMA(INTEGER* A1,REAL* A2 )
{ hnorma_( A1, A2 ); }

SUBROUTINE_F77 hopera_(int*,char*,int*,int*,float*,float*,int );
SUBROUTINE HOPERA(INTEGER* A1,CHARACTER A2,INTEGER* A3,INTEGER* A4,REAL* A5,REAL* A6 )
{ hopera_( A1, A2.rep, A3, A4, A5, A6, A2.len ); }

SUBROUTINE_F77 houtpu_(int* );
SUBROUTINE HOUTPU(INTEGER* A1 )
{ houtpu_( A1 ); }

SUBROUTINE_F77 hpagsz_(int* );
SUBROUTINE HPAGSZ(INTEGER* A1 )
{ hpagsz_( A1 ); }

SUBROUTINE_F77 hpak_(int*,float* );
SUBROUTINE HPAK(INTEGER* A1,REAL* A2 )
{ hpak_( A1, A2 ); }

SUBROUTINE_F77 hpakad_(int*,float* );
SUBROUTINE HPAKAD(INTEGER* A1,REAL* A2 )
{ hpakad_( A1, A2 ); }

SUBROUTINE_F77 hpake_(int*,float* );
SUBROUTINE HPAKE(INTEGER* A1,REAL* A2 )
{ hpake_( A1, A2 ); }

SUBROUTINE_F77 hparam_(int*,int*,float*,int*,double*,int*,int* );
SUBROUTINE HPARAM(INTEGER* A1,INTEGER* A2,REAL* A3,INTEGER* A4,DOUBLE_PRECISION* A5,INTEGER* A6,INTEGER* A7 )
{ hparam_( A1, A2, A3, A4, A5, A6, A7 ); }

SUBROUTINE_F77 hparmn_(float*,float*,float*,int*,int*,int*,float*,int*,double*,int*,int* );
SUBROUTINE HPARMN(REAL* A1,REAL* A2,REAL* A3,INTEGER* A4,INTEGER* A5,INTEGER* A6,REAL* A7,INTEGER* A8,DOUBLE_PRECISION* A9,INTEGER* A10,INTEGER* A11 )
{ hparmn_( A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11 ); }

SUBROUTINE_F77 hpchar_(int*,int* );
SUBROUTINE HPCHAR(INTEGER* A1,INTEGER* A2 )
{ hpchar_( A1, A2 ); }

SUBROUTINE_F77 hpdir_(char*,char*,int,int );
SUBROUTINE HPDIR(CHARACTER A1,CHARACTER A2 )
{ hpdir_( A1.rep, A2.rep, A1.len, A2.len ); }

SUBROUTINE_F77 hphist_(int*,char*,int*,int );
SUBROUTINE HPHIST(INTEGER* A1,CHARACTER A2,INTEGER* A3 )
{ hphist_( A1, A2.rep, A3, A2.len ); }

SUBROUTINE_F77 hphs_(int* );
SUBROUTINE HPHS(INTEGER* A1 )
{ hphs_( A1 ); }

SUBROUTINE_F77 hphst_(int* );
SUBROUTINE HPHST(INTEGER* A1 )
{ hphst_( A1 ); }

SUBROUTINE_F77 hponce_();
SUBROUTINE HPONCE()
{ hponce_(); }

SUBROUTINE_F77 hprint_(int* );
SUBROUTINE HPRINT(INTEGER* A1 )
{ hprint_( A1 ); }

SUBROUTINE_F77 hprnt_(int* );
SUBROUTINE HPRNT(INTEGER* A1 )
{ hprnt_( A1 ); }

SUBROUTINE_F77 hproj1_(int*,int*,int*,float*,int*,int*,int* );
SUBROUTINE HPROJ1(INTEGER* A1,INTEGER* A2,INTEGER* A3,REAL* A4,INTEGER* A5,INTEGER* A6,INTEGER* A7 )
{ hproj1_( A1, A2, A3, A4, A5, A6, A7 ); }

SUBROUTINE_F77 hproj2_(int*,int*,int*,float*,int*,int*,int*,int* );
SUBROUTINE HPROJ2(INTEGER* A1,INTEGER* A2,INTEGER* A3,REAL* A4,INTEGER* A5,INTEGER* A6,INTEGER* A7,INTEGER* A8 )
{ hproj2_( A1, A2, A3, A4, A5, A6, A7, A8 ); }

SUBROUTINE_F77 hprot_(int*,char*,int*,int );
SUBROUTINE HPROT(INTEGER* A1,CHARACTER A2,INTEGER* A3 )
{ hprot_( A1, A2.rep, A3, A2.len ); }

SUBROUTINE_F77 hpscat_(int* );
SUBROUTINE HPSCAT(INTEGER* A1 )
{ hpscat_( A1 ); }

SUBROUTINE_F77 hptab_(int* );
SUBROUTINE HPTAB(INTEGER* A1 )
{ hptab_( A1 ); }

SUBROUTINE_F77 hquad_(int*,char*,int*,float*,float*,int*,float*,int*,float*,float*,int*,int );
SUBROUTINE HQUAD(INTEGER* A1,CHARACTER A2,INTEGER* A3,REAL* A4,REAL* A5,INTEGER* A6,REAL* A7,INTEGER* A8,REAL* A9,REAL* A10,INTEGER* A11 )
{ hquad_( A1, A2.rep, A3, A4, A5, A6, A7, A8, A9, A10, A11, A2.len ); }

SUBROUTINE_F77 hrdir_(int*,char*,int*,int );
SUBROUTINE HRDIR(INTEGER* A1,CHARACTER A2,INTEGER* A3 )
{ hrdir_( A1, A2.rep, A3, A2.len ); }

SUBROUTINE_F77 hrebin_(int*,float*,float*,float*,float*,int*,int*,int* );
SUBROUTINE HREBIN(INTEGER* A1,REAL* A2,REAL* A3,REAL* A4,REAL* A5,INTEGER* A6,INTEGER* A7,INTEGER* A8 )
{ hrebin_( A1, A2, A3, A4, A5, A6, A7, A8 ); }

SUBROUTINE_F77 hrecov_(int*,char*,int );
SUBROUTINE HRECO(INTEGER* A1,CHARACTER A2 )
{ hrecov_( A1, A2.rep, A2.len ); }

SUBROUTINE_F77 hrend_(char*,int );
SUBROUTINE HREND(CHARACTER A1 )
{ hrend_( A1.rep, A1.len ); }

SUBROUTINE_F77 hreset_(int*,char*,int );
SUBROUTINE HRESET(INTEGER* A1,CHARACTER A2 )
{ hreset_( A1, A2.rep, A2.len ); }

SUBROUTINE_F77 hrfile_(int*,char*,char*,int,int );
SUBROUTINE HRFILE(INTEGER* A1,CHARACTER A2,CHARACTER A3 )
{ hrfile_( A1, A2.rep, A3.rep, A2.len, A3.len ); }

SUBROUTINE_F77 hrget_(int*,char*,char*,int,int );
SUBROUTINE HRGET(INTEGER* A1,CHARACTER A2,CHARACTER A3 )
{ hrget_( A1, A2.rep, A3.rep, A2.len, A3.len ); }

SUBROUTINE_F77 hrin_(int*,int*,int* );
SUBROUTINE HRIN(INTEGER* A1,INTEGER* A2,INTEGER* A3 )
{ hrin_( A1, A2, A3 ); }

SUBROUTINE_F77 hrndm2_(int*,float*,float* );
SUBROUTINE HRNDM2(INTEGER* A1,REAL* A2,REAL* A3 )
{ hrndm2_( A1, A2, A3 ); }

SUBROUTINE_F77 hropen_(int*,char*,char*,char*,int*,int*,int,int,int );
SUBROUTINE HROPEN(INTEGER* A1,CHARACTER A2,CHARACTER A3,CHARACTER A4,INTEGER* A5,INTEGER* A6 )
{ hropen_( A1, A2.rep, A3.rep, A4.rep, A5, A6, A2.len, A3.len, A4.len ); }

SUBROUTINE_F77 hrout_(int*,int*,char*,int );
SUBROUTINE HROUT(INTEGER* A1,INTEGER* A2,CHARACTER A3 )
{ hrout_( A1, A2, A3.rep, A3.len ); }

SUBROUTINE_F77 hrput_(int*,char*,char*,int,int );
SUBROUTINE HRPUT(INTEGER* A1,CHARACTER A2,CHARACTER A3 )
{ hrput_( A1, A2.rep, A3.rep, A2.len, A3.len ); }

SUBROUTINE_F77 hscale_(int*,float* );
SUBROUTINE HSCALE(INTEGER* A1,REAL* A2 )
{ hscale_( A1, A2 ); }

SUBROUTINE_F77 hscr_(int*,int*,char*,int );
SUBROUTINE HSCR(INTEGER* A1,INTEGER* A2,CHARACTER A3 )
{ hscr_( A1, A2, A3.rep, A3.len ); }

SUBROUTINE_F77 hsetpr_(char*,float*,int );
SUBROUTINE HSETPR(CHARACTER A1,REAL* A2 )
{ hsetpr_( A1.rep, A2, A1.len ); }

SUBROUTINE_F77 hsmoof_(int*,int*,float* );
SUBROUTINE HSMOOF(INTEGER* A1,INTEGER* A2,REAL* A3 )
{ hsmoof_( A1, A2, A3 ); }

SUBROUTINE_F77 hspli1_(int*,int*,int*,int*,float* );
SUBROUTINE HSPLI1(INTEGER* A1,INTEGER* A2,INTEGER* A3,INTEGER* A4,REAL* A5 )
{ hspli1_( A1, A2, A3, A4, A5 ); }

SUBROUTINE_F77 hspli2_(int*,int*,int*,int*,int* );
SUBROUTINE HSPLI2(INTEGER* A1,INTEGER* A2,INTEGER* A3,INTEGER* A4,INTEGER* A5 )
{ hspli2_( A1, A2, A3, A4, A5 ); }

SUBROUTINE_F77 hsquez_(char*,int );
SUBROUTINE HSQUEZ(CHARACTER A1 )
{ hsquez_( A1.rep, A1.len ); }

SUBROUTINE_F77 htitle_(char*,int );
SUBROUTINE HTITLE(CHARACTER A1 )
{ htitle_( A1.rep, A1.len ); }

SUBROUTINE_F77 hunpak_(int*,float*,char*,int*,int );
SUBROUTINE HUNPAK(INTEGER* A1,REAL* A2,CHARACTER A3,INTEGER* A4 )
{ hunpak_( A1, A2, A3.rep, A4, A3.len ); }

SUBROUTINE_F77 hunpke_(int*,float*,char*,int*,int );
SUBROUTINE HUNPKE(INTEGER* A1,REAL* A2,CHARACTER A3,INTEGER* A4 )
{ hunpke_( A1, A2, A3.rep, A4, A3.len ); }

SUBROUTINE_F77 huwfun_(int*,int*,char*,int*,char*,int,int );
SUBROUTINE HUWFUN(INTEGER* A1,INTEGER* A2,CHARACTER A3,INTEGER* A4,CHARACTER A5 )
{ huwfun_( A1, A2, A3.rep, A4, A5.rep, A3.len, A5.len ); }

SUBROUTINE_F77 hxi_(int*,float*,int* );
SUBROUTINE HXI(INTEGER* A1,REAL* A2,INTEGER* A3 )
{ hxi_( A1, A2, A3 ); }

SUBROUTINE_F77 hxyij_(int*,float*,float*,int*,int* );
SUBROUTINE HXYIJ(INTEGER* A1,REAL* A2,REAL* A3,INTEGER* A4,INTEGER* A5 )
{ hxyij_( A1, A2, A3, A4, A5 ); }

LOGICAL_FUNCTION_F77 hexist_(int*);
LOGICAL_FUNCTION HEXIST(INTEGER* A2)
{ return ( hexist_( A2 ) ); }

REAL_FUNCTION_F77 hi_(int*,int*);
REAL_FUNCTION HI(INTEGER* A2,INTEGER* A3)
{ return ( hi_( A2, A3 ) ); }

REAL_FUNCTION_F77 hie_(int*,int*);
REAL_FUNCTION HIE(INTEGER* A2,INTEGER* A3)
{ return ( hie_( A2, A3 ) ); }

REAL_FUNCTION_F77 hif_(int*,int*);
REAL_FUNCTION HIF(INTEGER* A2,INTEGER* A3)
{ return ( hif_( A2, A3 ) ); }

REAL_FUNCTION_F77 hij_(int*,int*,int*);
REAL_FUNCTION HIJ(INTEGER* A2,INTEGER* A3,INTEGER* A4)
{ return ( hij_( A2, A3, A4 ) ); }

REAL_FUNCTION_F77 hmax_(int*);
REAL_FUNCTION HMAX(INTEGER* A2)
{ return ( hmax_( A2 ) ); }

REAL_FUNCTION_F77 hmin_(int*);
REAL_FUNCTION HMIN(INTEGER* A2)
{ return ( hmin_( A2 ) ); }

REAL_FUNCTION_F77 hrndm1_(int*);
REAL_FUNCTION HRNDM1(INTEGER* A2)
{ return ( hrndm1_( A2 ) ); }

REAL_FUNCTION_F77 hspfun_(int*,float*,int*,int*);
REAL_FUNCTION HSFUN(INTEGER* A2,REAL* A3,INTEGER* A4,INTEGER* A5)
{ return ( hspfun_( A2, A3, A4, A5 ) ); }

REAL_FUNCTION_F77 hstati_(int*,int*,char*,int*,int);
REAL_FUNCTION HSTATI(INTEGER* A2,INTEGER* A3,CHARACTER A4,INTEGER* A5)
{ return ( hstati_( A2, A3, A4.rep, A5, A4.len ) ); }

REAL_FUNCTION_F77 hsum_(int*);
REAL_FUNCTION HSUM(INTEGER* A2)
{ return ( hsum_( A2 ) ); }

REAL_FUNCTION_F77 hx_(int*,float*);
REAL_FUNCTION HX(INTEGER* A2,REAL* A3)
{ return ( hx_( A2, A3 ) ); }

REAL_FUNCTION_F77 hxe_(int*,float*);
REAL_FUNCTION HXE(INTEGER* A2,REAL* A3)
{ return ( hxe_( A2, A3 ) ); }

REAL_FUNCTION_F77 hxy_(int*,float*,float*);
REAL_FUNCTION HXY(INTEGER* A2,REAL* A3,REAL* A4)
{ return ( hxy_( A2, A3, A4 ) ); }


#endif

// @endcode
// EOF
