#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hbook.h"


/** HLimit ******************************************/
void HLimit (Iword nwords)
{
  hlimit_(&nwords);
}

/** HROpen ******************************************/
void HROpen (Iword fileid, char *sdir, char *sfile, char *mode)
{
  hropen_(&fileid, sdir, sfile, mode, &record, &istat,
          strlen(sdir), strlen(sfile), 1);
}

/** HBook1 ******************************************/
void HBProf (Iword hid, char *title, 
             Iword chn, float xlow, float xup, float ymin, float ymax, 
             char *opt)
{
  hbprof_(&hid, title, &chn, &xlow, &xup, &ymin, &ymax, opt,
          strlen(title), strlen(opt));
}

/** HBook1 ******************************************/
void HBook1 (Iword hid, char *title, Iword chn, float min, float max)
{
  hbook1_(&hid, title, &chn, &min, &max, 
          &vmx, strlen(title));
}

/** HBook2 ******************************************/
void HBook2 (Iword hid, char *title, 
             Iword chnx, float minx, float maxx,
             Iword chny, float miny, float maxy)
{
  hbook2_(&hid, title, &chnx, &minx, &maxx, &chny, &miny, &maxy, 
          &vmx, strlen(title));
}

/** HFill1 ******************************************/
void HFill1 (Iword hid, float x, float w)
{
  hfill_(&hid, &x, &x, &w);
}

/** HFill2 ******************************************/
void HFill2 (Iword hid, float x, float y, float w)
{
  hfill_(&hid, &x, &y, &w);
}

/** HClose ******************************************/
void HClose (char *sdir)
{
  hrout_(&zero, &icycle, " ", 1);
  hrend_(sdir,strlen(sdir));
}

/** HBCwNT ******************************************/
void HBCwNT (Iword hid, char *title, char *opt)
{
  hbnt_(&hid, title, opt, strlen(title), strlen(opt));
}

/** HBName ******************************************/
void HBName (Iword hid, char *title, float *FirstVar, char *form)
{
  hbname_(&hid, title, FirstVar, form, strlen(title), strlen(form));
}

/** HFCwNT ******************************************/
void HFCwNT (Iword hid)
{
  hfnt_(&hid);
}

/** HBRwNT ******************************************/
void HBRwNT (Iword hid, char *title, Iword nvar, char *dir, char *varname)
{
  hbookn_(&hid, title, &nvar, dir, &record, varname, 
          strlen(title), strlen(dir), strlen(varname)/nvar);
}

/** HFRwNT ******************************************/
void HFRwNT (Iword hid, float *matrix)
{
  hfn_(&hid, matrix);
}

