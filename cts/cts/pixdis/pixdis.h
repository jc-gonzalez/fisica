/* prevent multiple includes
 */
#ifndef PIXDIS_H
#define PIXDIS_H 1

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
/*  defines from imager (must be consistent)
 */
#define cBITTEMPLATE "PMTBIT%d"  /* template to create Bitmap variable names */
#define iPMTBITNUM 4             /* number of Bitmap variables in ntuple */
#define iMAXNUMPIX 127           /* number of pixels in camera */
#define iBITSPERBYTE 8
#define dPIXSIZE 0.25

/*
 *  functions
 *
 */
extern void vget_pix_bitmap (HBOOK_FILE *);


#endif /* PIXDIS_H */
