#include <string.h>
#include <stdio.h>
#include <math.h>
#include "ctshbook.h"
#include "ctshplot.h"
#include "ctsbase.h"


/*
 *  functions
 *
 */
/*
 * control routines
 */
void cts_iginit (int nwhigz)
{
  extern void iginit_ ();

  iginit_ (&nwhigz);
}

void cts_igend ()
{
  extern void igend_ ();

  igend_ ();
}

void cts_igsse (int ierrf, int kwtype)
{
  extern void igsse_ ();

  igsse_ (&ierrf, &kwtype);
}



/*
 * basic graphic routines
 */
void cts_iopks (int ierrf)
{
  extern void iopks_ ();

  iopks_ (&ierrf);
}

void cts_igsg (int kwkid)
{
  extern void igsg_ ();

  igsg_ (&kwkid);
}

void cts_igsa (int kwkid)
{
  extern void igsa_ ();

  igsa_ (&kwkid);
}

void cts_iopwk (int kwkid, int konid, int kwtype)
{
  extern void iopwk_ ();

  iopwk_ (&kwkid, &konid, &kwtype);
}

void cts_iclwk (int kwkid)
{
  extern void iclwk_ ();

  iclwk_ (&kwkid);
}

void cts_iacwk (int kwkid)
{
  extern void iacwk_ ();

  iacwk_ (&kwkid);
}

void cts_idawk (int kwkid)
{
  extern void idawk_ ();

  idawk_ (&kwkid);
}

void cts_iuwk (int kwkid, int irflg)
{
  extern void iuwk_ ();

  iuwk_ (&kwkid, &irflg);
}

void cts_iclrwk (int kwkid, int kofl)
{
  extern void iclrwk_ ();

  iclrwk_ (&kwkid, &kofl);
}

void cts_iswkwn (int kwkid, float xmin, float xmax, float ymin, float ymax)
{
  extern void iswkwn_ ();

  if (xmin > xmax || ymin > ymax || 0.f > xmin || 1.0 < xmax
      || 0.f > ymin || 1.0 < ymax)

    cts_merror ("%s: invalid set of parameters.\n", "cts_iswkwn");


  iswkwn_ (&kwkid, &xmin, &xmax, &ymin, &ymax);
}

void cts_iswkvp (int kwkid, float xmin, float xmax, float ymin, float ymax)
{
  extern void iswkvp_ ();

  if (xmin > xmax || ymin > ymax)
    cts_merror ("%s: invalid set of parameters.\n", "cts_iswkvp");


  iswkvp_ (&kwkid, &xmin, &xmax, &ymin, &ymax);
}

void cts_iswn (int nt, float xmin, float xmax, float ymin, float ymax)
{
  extern void iswn_ ();

  if (xmin > xmax || ymin > ymax)
    cts_merror ("%s: invalid set of parameters.\n", "cts_iswn");

  iswn_ (&nt, &xmin, &xmax, &ymin, &ymax);
}

void cts_isvp (int nt, float xmin, float xmax, float ymin, float ymax)
{
  extern void isvp_ ();

  if (xmin > xmax || ymin > ymax || 0.f > xmin || 1.0 < xmax
      || 0.f > ymin || 1.0 < ymax)

    cts_merror ("%s: invalid set of parameters.\n", "cts_isvp");


  isvp_ (&nt, &xmin, &xmax, &ymin, &ymax);
}

void cts_iselnt (int nt)
{
  extern void iselnt_ ();

  iselnt_ (&nt);
}

void cts_igrng (float xsize, float ysize)
{
  extern void igrng_ ();

  igrng_ (&xsize, &ysize);
}

void cts_igmeta (int lun, int kwtype)
{
  extern void igmeta_ ();

  igmeta_ (&lun, &kwtype);
}


/*
 * basic output primitives
 */
void cts_ipl (int n, float *x, float *y)
{
  extern void ipl_ ();

  ipl_ (&n, x, y);
}


void cts_iml (int n, float *x, float *y)
{
  extern void iml_ ();

  iml_ (&n, x, y);
}


void cts_ipm (int n, float *x, float *y)
{
  extern void ipm_ ();

  ipm_ (&n, x, y);
}


void cts_ifa (int n, float *x, float *y)
{
  extern void ifa_ ();

  ifa_ (&n, x, y);
}


void cts_itx (float x, float y, char *chars)
{
  extern void itx_ ();

  itx_ (&x, &y, chars, strlen (chars));
}


/*
 * basic output attributes
 */
void cts_isclip (int iclsw)
{
  int iclsw2;
  extern void isclip_ ();

  /* since only the values '0' and '1' are allowed for iclsw
   * we take iclsw modulo 2 to set all values onto this range
   */
  iclsw2 = iclsw % 2;

  isclip_ (&iclsw2);
}


void cts_iscr (int kwkid, int ici, float cr, float cg, float cb)
{
  extern void iscr_ ();

  iscr_ (&kwkid, &ici, &cr, &cg, &cb);
}


void cts_isplci (int icoli)
{
  extern void isplci_ ();

  isplci_ (&icoli);
}


void cts_ispmci (int icoli)
{
  extern void ispmci_ ();

  ispmci_ (&icoli);
}


void cts_isfaci (int icoli)
{
  extern void isfaci_ ();

  isfaci_ (&icoli);
}


void cts_istxci (int icoli)
{
  extern void istxci_ ();

  istxci_ (&icoli);
}


void cts_isfais (int ints)
{
  extern void isfais_ ();

  isfais_ (&ints);
}


void cts_isfasi (int istyli)
{
  extern void isfasi_ ();

  isfasi_ (&istyli);
}


void cts_isln (int ltype)
{
  extern void isln_ ();

  isln_ (&ltype);
}


void cts_islwsc (float width)
{
  extern void islwsc_ ();

  islwsc_ (&width);
}


void cts_ismk (int mtype)
{
  extern void ismk_ ();

  ismk_ (&mtype);
}


void cts_ismksc (float ssfm)
{
  extern void ismksc_ ();

  ismksc_ (&ssfm);
}


void cts_istxal (int itxalh, int itxalv)
{
  extern void istxal_ ();

  istxal_ (&itxalh, &itxalv);
}


void cts_ischh (float chh)
{
  extern void ischh_ ();

  ischh_ (&chh);
}


void cts_ischup (float rchux, float rchuy)
{
  extern void ischup_ ();

  ischup_ (&rchux, &rchuy);
}


void cts_istxfp (int ifont, int iprec)
{
  extern void istxfp_ ();

  istxfp_ (&ifont, &iprec);
}

void cts_igset (char *chname, float val)
{
  extern void igset_ ();

  igset_ (chname, &val, strlen (chname));
}

void cts_igqwk (int iwkid, char *pname, void *rval)
{
  extern void igqwk_ ();

  igqwk_ (&iwkid, pname, rval, strlen (pname));
}

void cts_igraph (int n, float *x, float *y, char *chopt)
{
  extern void igraph_ ();

  igraph_ (&n, x, y, chopt, strlen (chopt));
}

void cts_igaxis (float x0, float x1, float y0, float y1, float wmin, float wmax,
		 int ndiv, char *chopt)
{
  extern void igaxis_ ();

  igaxis_ (&x0, &x1, &y0, &y1, &wmin, &wmax, &ndiv, chopt, strlen (chopt));
}

void cts_igtext (float x, float y, char *text, float size, float angle,
		 char *chopt)
{
  extern void igtext_ ();

  igtext_ (&x, &y, text, &size, &angle, chopt, strlen (text), strlen (chopt));
}


void cts_igzset (char *chopt)
{
  extern void igzset_ ();

  igzset_ (chopt, strlen (chopt));
}





/*
 * HPLOT CALLS
 *
 */
void cts_hplabl (int num, int nb, char *chlab)
{
  extern void hplabl_ ();

  hplabl_ (&num, &nb, chlab, strlen (chlab));
}


void cts_hplaer (float *xu, float *yu, float *dxu1, float *dxu2, float *dyu1,
		 float *dyu2, int n, char chopt, int isym, float usize)
{
  char popt[2] = " ";
  extern void hplaer_ ();

  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hplaer_ (xu, yu, dxu1, dxu2, dyu1, dyu2, &n, popt, &isym, &usize);
}


void cts_hplarc (float xc, float yc, float rad, float phi1, float phi2)
{
  extern void hplarc_ ();

  hplarc_ (&xc, &yc, &rad, &phi1, &phi2);
}


void cts_hplax (char *chxtit, char *chytit)
{
  extern void hplax_ ();

  hplax_ (chxtit, chytit, strlen (chxtit), strlen (chytit));
}


void cts_hplbox (float xlow, float ylow, float xup, float yup, char chopt)
{
  char popt[2] = " ";
  extern void hplbox_ ();

  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hplbox_ (&xlow, &ylow, &xup, &yup, popt, strlen (popt));
}


void cts_hplcap (int ifile)
{
  extern void hplcap_ ();

  hplcap_ (&ifile);
}


void cts_hplcom (float xm, float ym, char *chtit)
{
  extern void hplcom_ ();

  hplcom_ (&xm, &ym, chtit, strlen (chtit));
}


void cts_hplcon (int id, int nlevel, int iflag)
{
  extern void hplcon_ ();

  hplcon_ (&id, &nlevel, &iflag);
}


void cts_hpldo (int lun)
{
  extern void hpldo_ ();

  hpldo_ (&lun);
}


void cts_hplego (int id, float theta, float phi)
{
  extern void hplego_ ();

  hplego_ (&id, &theta, &phi);
}


void cts_hplend ()
{
  extern void hplend_ ();

  hplend_ ();
}


void cts_hplerr (float *xu, float *yu, float *dxu, float *dyu, int n,
		 char chopt, int isym, float usize)
{
  char popt[2] = " ";
  extern void hplerr_ ();

  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hplerr_ (xu, yu, dxu, dyu, &n, popt, &isym, &usize, strlen (popt));
}



void cts_hplfra (float x1, float x2, float y1, float y2, char *popt)
{
  extern void hplfra_ ();

  hplfra_ (&x1, &x2, &y1, &y2, popt, strlen (popt));
}


void cts_hplfun (float *xu, float *yu, int n, char chopt)
{
  char popt[2] = " ";
  extern void hplfun_ ();

  /* check for valid range of n (0 < n < 1002)
   */
  if (0 > n || n > 1002)
    cts_merror ("cts_hplfun: invalid number of points '%d'.\n", n);


  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hplfun_ (xu, yu, &n, popt, strlen (popt));
}


void cts_hplgiv (float xl, float yl, float xh, float yh)
{
  extern void hplgiv_ ();

  hplgiv_ (&xl, &yl, &xh, &yh);
}


void cts_hpline (float *xu, float *yu, int n, char chopt)
{
  char popt[2] = " ";
  extern void hpline_ ();

  /* check for valid range of n (0 < n < 1002)
   */
  if (0 > n || n > 1002)
    cts_merror ("cts_hpline: invalid number of points %d.\n", n);


  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hpline_ (xu, yu, &n, popt, strlen (popt));
}


void cts_hplint (iwtyp)
{
  extern void hplint_ ();

  hplint_ (&iwtyp);
}


void cts_hplkey (float xc, float yc, int isym, char *chtit)
{
  extern void hplkey_ ();

  hplkey_ (&xc, &yc, &isym, chtit, strlen (chtit));
}


void cts_hplnt (int idn, int isel, float (*uwfunc) (), int ifrom, int ito,
	    int ivarx, int ivary)
{
  extern void hplnt_ ();

  hplnt_ (&idn, &isel, uwfunc, &ifrom, &ito, &ivarx, &ivary);
}


void cts_hplnul ()
{
  extern void hplnul_ ();

  hplnul_ ();
}


void cts_hploc (int ntpri, int ntloc, float xloc, float yloc, int idh, int icx,
	    int icy, int istat)
{
  extern void hploc_ ();

  hploc_ (&ntpri, &ntloc, &xloc, &yloc, &idh, &icx, &icy, &istat);
}


void cts_hplopt (char chopt[][iCOPT_LEN], int n)
{
  extern void hplopt_ ();

  hplopt_ (chopt, &n, iCOPT_LEN);
}


void cts_hplot (int id, char *chopt, char *chcase, int num)
{
  char pchase[5] = "    ";
  extern void hplot_ ();

  /* maximum allowed size for chcase is 4 characters (+ '\0')
   */
  strncpy (pchase, chcase, (size_t) 4);
  pchase[5] = NUL;

  hplot_ (&id, chopt, pchase, &num, strlen (chopt), strlen (pchase));
}


void cts_hplpro (int id, char *chxtit, char *chytit)
{
  extern void hplpro_ ();

  hplpro_ (&id, chxtit, chytit, strlen (chxtit), strlen (chytit));
}


void cts_hplpto (char *strout, char *strin)
{
  extern void hplpto_ ();

  hplpto_ (strout, strin, strlen (strout), strlen (strin));
}


void cts_hplset (char *chopt, float var)
{
  char pchopt[5] = "    ";
  extern void hplset_ ();

  /* maximum allowed size for chopt is 4 characters (+ '\0')
   */
  strncpy (pchopt, chopt, (size_t) 4);
  pchopt[5] = NUL;

  hplset_ (pchopt, &var, strlen (pchopt));
}


void cts_hplsiz (float xsize, float ysize, char chopt)
{
  char popt[2] = " ";
  extern void hplsiz_ ();

  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hplsiz_ (&xsize, &ysize, popt, strlen (popt));
}


void cts_hplsof (float x, float y, char *chtxt, float size, float angle,
		 float dummy, int iopt)
{
  extern void hplsof_ ();

  hplsof_ (&x, &y, chtxt, &size, &angle, &dummy, &iopt, strlen (chtxt));
}


void cts_hplsur (int id, float theta, float phi, int dummy)
{
  extern void hplsur_ ();

  hplsur_ (&id, &theta, &phi, &dummy);
}


void cts_hplsym (float *x, float *y, int n, int isym, float usize, char chopt)
{
  char popt[2] = " ";
  extern void hplsym_ ();

  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hplsym_ (x, y, &n, &isym, &usize, popt, strlen (popt));
}


void cts_hpltab (int id, int npar, float *par, char chopt)
{
  char popt[2] = " ";
  extern void hpltab_ ();

  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hpltab_ (&id, &npar, par, popt, strlen (popt));
}


void cts_hpltit (char *chtit)
{
  size_t strsize;
  char pchtit[iTITLELEN] = " ";
  extern void hpltit_ ();

  /* maximum allowed size for chtit is 80 characters; since I don't
   * know whether '\0' is included, I restrict it to 79 chars
   */
  strsize = strlen (chtit);

  if (strsize >= iTITLELEN)
    {
      strncpy (pchtit, chtit, (size_t) (iTITLELEN - 1));
      pchtit[iTITLELEN] = NUL;

      chtit = pchtit;
      strsize = iTITLELEN;
    }

  hpltit_ (chtit, strsize);
}


void cts_hplwir (char chopt, float xval, float yval, char *chtick)
{
  char popt[2] = " ";
  char pchtick[5] = " ";
  extern void hplwir_ ();

  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  /* maximum allowed size for chtick is 4 characters (+ '\0')
   */
  strncpy (pchtick, chtick, (size_t) 4);
  pchtick[5] = NUL;

  hplwir_ (popt, &xval, &yval, pchtick, strlen (popt), strlen (pchtick));
}


void cts_hplzom (int id, char *chopt, int imin, int imax)
{
  extern void hplzom_ ();

  hplzom_ (&id, chopt, &imin, &imax, strlen (chopt));
}


void cts_hplzon (int nxzon, int nyzon, int ifirst, char chopt)
{
  char popt[2] = " ";
  extern void hplzon_ ();

  /* pass chopt as null terminated string
   */
  sprintf (popt, "%c", chopt);

  hplzon_ (&nxzon, &nyzon, &ifirst, popt, strlen (popt));
}


void cts_vset_def ()
{
  char chopt[4][iCOPT_LEN] = {{"NBOX"}, {"NGRI"}, {"NSTA"}, {"UTIT"}};

  /* set fonts
   */
  cts_igset ("TXFP", -60.);
  cts_hplset ("CFON", -60.);
  cts_hplset ("GFON", -60.);
  cts_hplset ("LFON", -60.);
  cts_hplset ("TFON", -60.);
  cts_hplset ("VFON", -60.);

  /* line width
   */
  cts_igset ("LWID", 4.);
  cts_hplset ("BWID", 5.);
  cts_hplset ("FWID", 6.);
  cts_hplset ("HWID", 5.);
  cts_hplset ("XWID", 5.);
  cts_hplset ("YWID", 5.);

  /* scales and measures
   */
  cts_igset ("CHHE", 0.02);
  cts_hplset ("ASIZ", 0.3);
  cts_hplset ("GSIZ", 0.7);
  cts_hplset ("HMAX", 0.75);
  cts_hplset ("KSIZ", 0.15);
  cts_hplset ("VSIZ", 0.3);
  cts_hplset ("XLAB", 1.5);
  cts_hplset ("XMGL", 3.0);
  cts_hplset ("XMGR", 1.5);
  cts_hplset ("XVAL", 0.4);
  cts_hplset ("YHTI", 1.4);
  cts_hplset ("YLAB", 1.);
  cts_hplset ("YMGL", 2.4);
  cts_hplset ("YMGU", 2.0);
  cts_hplset ("YVAL", 0.4);

  cts_hplopt (chopt, 4);
}

void cts_vinit_hplot (int iwtyp)
{
  /* ZEBRA is initialized by a call to the hbook-init routine
   */
  cts_vinit_hbook ();

  /* init hplot package
   */
  cts_hplint (iwtyp);

  /* load default settings
   */
  cts_vset_def ();
}
