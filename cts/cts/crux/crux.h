/* prevent multiple includes
 */
#ifndef CRUX_H
#define CRUX_H 1


#ifndef __cplusplus

/* we're not dealing with C++
 * if true was not defined before define boolean stuff
 */
#ifndef true
typedef int bool;
#define true 1
#define false 0
#endif

#endif


/*
 *  defines
 *
 */
#define iNTUPLE_PARS  13
#define iEFUNC_PARS 5
#define iIFUNC_PARS 4
#define iNIEBINS 9

#define fMINDIST 0.f
#define fMAXDIST 1.5f

#define cNTUP_NAM_OUT "crux_out.hbook"
#define cEHISTO_TIT   ";(E?r! - E?mc!) / E?mc!;entries"
#define cIHISTO_TIT   ";(I?r! - I?mc!) / I?mc!;entries"
#define cERES_TIT     ";RMS((E?r!-E?mc!) / E?mc!), MEAN((E?r!-E?mc!) / E?mc!)"
#define cIRES_TIT     ";RMS((I?r!-I?mc!) / I?mc!), MEAN((I?r!-I?mc!) / I?mc!)"
#define cSUBTITEMC    "%%6.2f \"L# E?mc! \"M#TeV\"N# \"L# %6.2f"
#define cSUBTITER     "%%6.2f \"L# E?r! \"M#TeV\"N# \"L# %6.2f"
#define cSUBTITIMC    "%%6.1f \"L# I?mc! \"M#m\"N# \"L# %6.1f"
#define cSUBTITIR     "%%6.1f \"L# I?r! \"M#m\"N# \"L# %6.1f"


/*
 *  structures
 *
 */
/* struct to containing data to derive energy and impact parameter
 * resolution
 */
typedef struct ie_res_list
{
  float fza;                    /* zenith angle */
  float fde;                    /* (E_mc - E_est) / E_mc */
  float fdi;                    /* (I_mc - I_est) / I_mc */
  float fweight;                /* weight to simulate a given spectrum */
  struct ie_res_list *p2next;
} IERESLIST;

typedef struct ie_res_list IE_LIST_EL;


/*
 *  functions
 *
 */
extern void vget_efunc (ELIST_EL *, double **, double **);
extern void vget_ifunc (ELIST_EL *, double **, double **);

extern void vplot_eires_e (HBOOK_FILE *, bool);
extern void vplot_eires_i (HBOOK_FILE *, bool);

/*
 *  macros
 *
 */
/* this macro returns the estimated energy for a given event
 * and set of fit parameters
 * a - ELIST_EL event-struct
 * b - array of fit parameters
 * c - estimated energy
 */
#define menergy(a, b, c)                                                       \
 do {                                                                          \
     c = (b[0] + b[1] * a->ddens + b[2] * a->di_est + b[3] * a->dlength         \
          + b[4] * a->dlength * a->ddens) * (1. / m2 (a->dcza)                 \
          + 0.2 / m4 (a->dcza)) / 1.2;                                         \
    }                                                                          \
 while (0)

/* this macro returns the estimated impact parameter for a given event
 * and set of fit parameters
 * a - ELIST_EL event-struct
 * b - array of fit parameters
 * c - estimated impact parameter
 */
#define mimpact(a, b, c)                                                       \
 do {                                                                          \
     c  = (b[0] + b[1] * a->ddist + b[2] * a->dwidth)  * (1. / a->dcza         \
           + b[3] / m2 (a->dcza)) / (1. + b[3]);                               \
    }                                                                          \
 while (0)

/* get event number b from list a
 */
#define mget_el(a,b)                                                           \
 do {int mi = 1;                                                               \
     while (mi++ < (int) b)                                                    \
       a = a->p2next;                                                          \
    }                                                                          \
 while (0)


#endif /* CRUX_H */
