/* prevent multiple includes
 */
#ifndef SOURCE2D_H
#define SOURCE2D_H 1


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
/* id's of histograms
 */
#define iHTS_ON_ID           100
#define iHTS_OFF_ID          110
#define iHTS_OFF_N_ID        120
#define iHTS_ID              130
#define iHTS_OFF_BIN_ID      140
#define iHALPHA_ON_ID        200
#define iHALPHA_ON2_ID       201
#define iHALPHA_OFF_ID       210
#define iHALPHA_OFF2_ID      211
#define iHALPHA_OFF_N_ID     220
#define iHALPHA_OFF_BIN_ID   240
#define iHSIG_ON_ID          1000
#define iHSIG_OFF_ID         1010
#define iHSIG_OFF_N_ID       1020
#define iHSIG_OFF_BIN_ID     1040
#define iHSIG_ID             2000
#define iHSIG_ZOOM_ID        2010
#define iHSIG_OFF_N_ZOOM_ID  2020


/* histogram titles
 */
#define sHSIG_TIT       "ON - OFF norm.; x \"M# deg. \"N#; y \"M# deg. \"N#;"
#define sHSIG_ZOOM_TIT  "ON - OFF norm.; x \"M# deg. \"N#; y \"M# deg. \"N#;"
#define sHSIG_ON_TIT    "ON; x \"M# deg. \"N#; y \"M# deg. \"N#;"
#define sHSIG_OFF_TIT   "OFF; x \"M# deg. \"N#; y \"M# deg. \"N#;"
#define sHSIG_OFF_N_TIT "OFF norm.; x \"M# deg. \"N#; y \"M# deg. \"N#;"
#define sHTS_TIT        "ON - OFF; [Q]^2! \"M# deg. ^2! \"N#; No. of events"
#define sHTS_ON_TIT     "ON; [Q]^2! \"M# deg. ^2! \"N#; No. of events"
#define sHTS_OFF_TIT    "OFF; [Q]^2! \"M# deg. ^2! \"N#; No. of events"
#define sHTS_OFF_N_TIT  "OFF norm.; [Q]^2! \"M# deg. ^2! \"N#; No. of events"
#define sHA_ON_TIT      "ON; ALPHA \"M# deg. \"N#; No. of events"
#define sHA_ON2_TIT     "ON; signed ALPHA \"M# deg. \"N#; No. of events"
#define sHA_OFF_TIT     "OFF; ALPHA \"M# deg. \"N#; No. of events"
#define sHA_OFF2_TIT    "OFF; signed ALPHA \"M# deg. \"N#; No. of events"
#define sHA_OFF_N_TIT   "OFF norm.; ALPHA \"M# deg. \"N#; No. of events"


/* histogram boundarys and number of bins
 */
#define f2D_XLO -1.75f
#define f2D_XHI  1.75f
#define f2D_YLO -1.75f
#define f2D_YHI  1.75f
#define i2D_BINX 70
#define i2D_BINY 70
#define f2D_Z_XLO -0.5f
#define f2D_Z_XHI  0.5f
#define f2D_Z_YLO -0.5f
#define f2D_Z_YHI  0.5f
#define i2D_Z_BINX 10
#define i2D_Z_BINY 10
#define f1D_TS_XLO  0.f
#define f1D_TS_XHI  1.f
#define i1D_TS_BINX 100
#define f1D_A_XLO  0.f
#define f1D_A_XHI  90.f
#define i1D_A_BINX 18
#define f1D_A2_XLO  -90.f
#define f1D_A2_XHI   90.f
#define i1D_A2_BINX  36


/* some cut values
 */
#define dALPHA_CUT_FS  5.f


/* strings related to the false-source option
 * sFS_STRING   - this string has to be passed on the command line if
 *                the false-source plot should be produced
 * sFS_HSIG_STR - this string changes the title of the HSIG histogram
 *                (to show that the false source method was used)
 */
#define sFS_STRING "-f"
#define sFS_HSIG_STR "fs: ON - OFF norm.; x \"M# deg. \"N#; y \"M# deg. \"N#;"


#endif /* SOURCE2D_H */
