#ifndef __HBOOK_PROTOTYPES_C__
#  define __HBOOK_PROTOTYPES_C__

#define HBOOKW      1000000
#define Iword       int

void HLimit (Iword nwords);
void HROpen (Iword fileid, char *sdir, char *sfile, char *mode);
void HBProf (Iword hid, char *title, 
             Iword chn, float xlow, float xup, float ymin, float ymax, 
             char *opt);
void HBook1 (Iword hid, char *title, Iword chn, float min, float max);
void HBook2 (Iword hid, char *title, 
             Iword chnx, float minx, float maxx,
             Iword chny, float miny, float maxy);
void HFill1 (Iword hid, float x, float w);
void HFill2 (Iword hid, float x, float y, float w);
void HClose (char *sdir);
void HBCwNT (Iword hid, char *title, char *opt);
void HBName (Iword hid, char *title, float *FirstVar, char *form);
void HFCwNT (Iword hid);
void HBRwNT (Iword hid, char *title, Iword nvar, char *dir, char *varname);
void HFRwNT (Iword hid, float *matrix);

#endif

#ifndef __MAIN__
Iword           pawc_[HBOOKW];
Iword           vmx = 0L;
Iword           istat, record = 1024L, icycle=9999999L;
float           zero = 0.0, one = 1.0;
#endif

