/*
 * Numerical Recipes functions
 * (adopted amoeba.c and amotry to my needs)
 *
 *
 */
#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "nr.h"


static void nrerror (char error_text[])

  /* Numerical Recipes standard error handler
   */
{
  fprintf (stderr, "Numerical Recipes run-time error...\n");
  fprintf (stderr, "%s\n", error_text);
  fprintf (stderr, "...now exiting to system...\n");
  exit (iERRCODE);
}


double gamma (double xx)

  /* changed from gammln to gamma - D.K.
   */
{
	double x, y, tmp, ser;
	static double cof[6] = {76.18009172947146, -86.50532032941677,
		24.01409824083091, -1.231739572450155,
		0.1208650973866179e-2, -0.5395239384953e-5};
	int j;

	y = x = xx;
	tmp = (x + 5.5);
	tmp = exp ((x + 0.5) * log (tmp) - tmp);

	ser = 1.000000000190015;
	for (j = 0; j <= 5; j++)
	  ser += cof[j] / ++y;

	return (tmp * 2.5066282746310005 * ser / x);
}


double *vector (long nl, long nh)

  /* allocate a double vector with subscript range v[nl..nh]
   * (adopted to cts_mmalloc macro - D.K.)
   */
{
  double *v;

  v = cts_mmalloc (double, (nh - nl + 1 + iNR_END), "vector");

  return (v - nl + iNR_END);
}


int *ivector (long nl, long nh)

  /* allocate an int vector with subscript range v[nl..nh]
   * (adopted to my cts_mmalloc macro - D.K.)
   */
{
  int *v;

  v = cts_mmalloc (int, (nh - nl + 1 + iNR_END), "ivector");
  return v - nl + iNR_END;
}


double **matrix (long nrl, long nrh, long ncl, long nch)

  /* allocate a double matrix with subscript range m[nrl..nrh][ncl..nch]
   * (adopted to my cts_mmalloc macro - D.K.)
   */
{
  long i, nrow = nrh - nrl + 1, ncol = nch - ncl + 1;
  double **m;

  /* allocate pointers to rows
   */
  m = cts_mmalloc (double *, (nrow + iNR_END), "matrix");

  m += iNR_END;
  m -= nrl;

  /* allocate rows and set pointers to them
   */
  m[nrl] = cts_mmalloc (double, (nrow * ncol + iNR_END), "matrix");

  m[nrl] += iNR_END;
  m[nrl] -= ncl;

  for (i = nrl+1; i <= nrh; i++)
    m[i] = m[i-1] + ncol;

  /* return pointer to array of pointers to rows */
  return (m);
}


static double amotry (double **p, double y[], double psum[], int ndim,
		      double (*func) (double []), int ihi, double dac)

{
  int j;
  double dac1, dac2, ytry;

  static int indim_old = 0;
  static double *ptry = NULL;


  /* added code - D.K.
   * vector is created only if necessary (function is entered for the first time
   * or old vector is too small)
   */
  if (indim_old < ndim)
    {
      ptry = vector (1, ndim);
      indim_old = ndim;
    }
  else if (ndim <= 0)
    cts_merror ("%d: invalid dimension!\n", ndim);


  dac1 = (1.0 - dac) / ndim;
  dac2 = dac1 - dac;
  for (j = 1; j <= ndim; j++)
    ptry[j] = psum[j] * dac1 - p[ihi][j] * dac2;

  ytry = (*func) (ptry);
  if (ytry < y[ihi])
    {
      y[ihi] = ytry;
      for (j = 1; j <= ndim; j++)
	{
	  psum[j] += ptry[j] - p[ihi][j];
	  p[ihi][j] = ptry[j];
	}
    }

  return (ytry);
}


void amoeba (double **p, double y[], int ndim, double ftol, double (*func) (double []),
	     int *nfunc)

{
  int i, ihi, ilo, inhi, j, mpts = ndim + 1;
  double rtol, sum, swap, ysave, ytry;

  static int indim_old = 0;
  static double *psum = NULL;


  /* added code - D.K.
   * vector is created only if necessary (function is entered for the first time
   * or old vector is too small)
   */
  if (indim_old < ndim)
    {
      psum = vector (1, ndim);
      indim_old = ndim;
    }
  else if (ndim <= 0)
    cts_merror ("%d: invalid dimension!\n", ndim);


  *nfunc = 0;
  GET_PSUM;

  for (;;)
    {
      ilo = 1;
      ihi = (y[1] > y[2]) ? (inhi = 2,1) : (inhi = 1,2);

      for (i = 1; i <= mpts; i++)
	{
	  if (y[i] <= y[ilo]) ilo=i;
	  if (y[i] > y[ihi])
	    {
	      inhi = ihi;
	      ihi = i;
	    }
	  else if (y[i] > y[inhi] && i != ihi)
	    inhi=i;
	}

      rtol = 2.0 * fabs (y[ihi] - y[ilo]) / (fabs (y[ihi]) + fabs (y[ilo]) + iTINY);

      if (rtol < ftol)
	{
	  SWAP (y[1], y[ilo]);
	  for (i = 1; i <= ndim; i++)
	    SWAP (p[1][i], p[ilo][i]);
	  break;
	}

      if (*nfunc >= iNMAX)
	/* changed error handler, D.K.
	 */
	{
	  *nfunc = -*nfunc;
	  break;
	}

      *nfunc += 2;
      ytry = amotry (p, y, psum, ndim, func, ihi, -1.0);

      if (ytry <= y[ilo])
	ytry = amotry (p, y, psum, ndim, func, ihi, 2.0);

      else if (ytry >= y[inhi])
	{
	  ysave = y[ihi];
	  ytry = amotry (p, y, psum, ndim, func, ihi, 0.5);

	  if (ytry >= ysave)
	    {
	      for (i = 1; i <= mpts; i++)
		{
		  if (i != ilo)
		    {
		      for (j = 1; j <= ndim; j++)
			p[i][j] = psum[j] = 0.5 * (p[i][j] + p[ilo][j]);
		      y[i] = (*func) (psum);
		    }
		}

	      *nfunc += ndim;
	      GET_PSUM;
	    }
	}

      else
	--(*nfunc);
    }
}


#define IA 16807
#define IM 2147483647
#define AM (1.0/IM)
#define IQ 127773
#define IR 2836
#define NTAB 32
#define NDIV (1+(IM-1)/NTAB)
#define EPS 1.2e-7
#define RNMX (1.0-EPS)

double ran1(long *idum)
{
	int j;
	long k;
	static long iy=0;
	static long iv[NTAB];
	double temp;

	if (*idum <= 0 || !iy) {
		if (-(*idum) < 1) *idum=1;
		else *idum = -(*idum);
		for (j=NTAB+7;j>=0;j--) {
			k=(*idum)/IQ;
			*idum=IA*(*idum-k*IQ)-IR*k;
			if (*idum < 0) *idum += IM;
			if (j < NTAB) iv[j] = *idum;
		}
		iy=iv[0];
	}
	k=(*idum)/IQ;
	*idum=IA*(*idum-k*IQ)-IR*k;
	if (*idum < 0) *idum += IM;
	j=iy/NDIV;
	iy=iv[j];
	iv[j] = *idum;
	if ((temp=AM*iy) > RNMX) return RNMX;
	else return temp;
}
#undef IA
#undef IM
#undef AM
#undef IQ
#undef IR
#undef NTAB
#undef NDIV
#undef EPS
#undef RNMX

#define IM1 2147483563
#define IM2 2147483399
#define AM (1.0/IM1)
#define IMM1 (IM1-1)
#define IA1 40014
#define IA2 40692
#define IQ1 53668
#define IQ2 52774
#define IR1 12211
#define IR2 3791
#define NTAB 32
#define NDIV (1+IMM1/NTAB)
#define EPS 1.2e-7
#define RNMX (1.0-EPS)

double ran2(long *idum)
{
	int j;
	long k;
	static long idum2=123456789;
	static long iy=0;
	static long iv[NTAB];
	double temp;

	if (*idum <= 0) {
		if (-(*idum) < 1) *idum=1;
		else *idum = -(*idum);
		idum2=(*idum);
		for (j=NTAB+7;j>=0;j--) {
			k=(*idum)/IQ1;
			*idum=IA1*(*idum-k*IQ1)-k*IR1;
			if (*idum < 0) *idum += IM1;
			if (j < NTAB) iv[j] = *idum;
		}
		iy=iv[0];
	}
	k=(*idum)/IQ1;
	*idum=IA1*(*idum-k*IQ1)-k*IR1;
	if (*idum < 0) *idum += IM1;
	k=idum2/IQ2;
	idum2=IA2*(idum2-k*IQ2)-k*IR2;
	if (idum2 < 0) idum2 += IM2;
	j=iy/NDIV;
	iy=iv[j]-idum2;
	iv[j] = *idum;
	if (iy < 1) iy += IMM1;
	if ((temp=AM*iy) > RNMX) return RNMX;
	else return temp;
}
#undef IM1
#undef IM2
#undef AM
#undef IMM1
#undef IA1
#undef IA2
#undef IQ1
#undef IQ2
#undef IR1
#undef IR2
#undef NTAB
#undef NDIV
#undef EPS
#undef RNMX

double gasdev(long *idum)
{
	double ran2(long *idum);
	static int iset=0;
	static double gset;
	double fac,rsq,v1,v2;

	if  (iset == 0) {
		do {
			v1=2.0*ran2(idum)-1.0;
			v2=2.0*ran2(idum)-1.0;
			rsq=v1*v1+v2*v2;
		} while (rsq >= 1.0 || rsq == 0.0);
		fac=sqrt(-2.0*log(rsq)/rsq);
		gset=v1*fac;
		iset=1;
		return v2*fac;
	} else {
		iset=0;
		return gset;
	}
}


#define NRANSI
#define GOLD 1.618034
#define GLIMIT 100.0
#define TINY 1.0e-20
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);

void mnbrak(double *ax, double *bx, double *cx, double *fa, double *fb,
	    double *fc,	double (*func)(double))
{
  double ulim,u,r,q,fu,dum;

  *fa=(*func)(*ax);
  *fb=(*func)(*bx);
  if (*fb > *fa) {
    SHFT(dum,*ax,*bx,dum)
      SHFT(dum,*fb,*fa,dum)
      }
  *cx=(*bx)+GOLD*(*bx-*ax);
  *fc=(*func)(*cx);
  while (*fb > *fc) {
    r=(*bx-*ax)*(*fb-*fc);
    q=(*bx-*cx)*(*fb-*fa);
    u=(*bx)-((*bx-*cx)*q-(*bx-*ax)*r)/
      (2.0*SIGN(max(fabs(q-r),TINY),q-r));
    ulim=(*bx)+GLIMIT*(*cx-*bx);
    if ((*bx-u)*(u-*cx) > 0.0) {
      fu=(*func)(u);
      if (fu < *fc) {
	*ax=(*bx);
	*bx=u;
	*fa=(*fb);
	*fb=fu;
	return;
      } else if (fu > *fb) {
	*cx=u;
	*fc=fu;
	return;
      }
      u=(*cx)+GOLD*(*cx-*bx);
      fu=(*func)(u);
    } else if ((*cx-u)*(u-ulim) > 0.0) {
      fu=(*func)(u);
      if (fu < *fc) {
	SHFT(*bx,*cx,u,*cx+GOLD*(*cx-*bx))
	  SHFT(*fb,*fc,fu,(*func)(u))
	  }
    } else if ((u-ulim)*(ulim-*cx) >= 0.0) {
      u=ulim;
      fu=(*func)(u);
    } else {
      u=(*cx)+GOLD*(*cx-*bx);
      fu=(*func)(u);
    }
    SHFT(*ax,*bx,*cx,u)
      SHFT(*fa,*fb,*fc,fu)
      }
}


#undef GOLD
#undef GLIMIT
#undef TINY
#undef SHFT
#undef NRANSI

#define NRANSI
#define ITMAX 100
#define CGOLD 0.3819660
#define ZEPS 1.0e-10
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);


double brent (double ax, double bx, double cx, double (*f)(double),
	      double tol, double *xmin)
{
  int iter;
  double a,b,d=0.,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
  double e=0.0;

  a=(ax < cx ? ax : cx);
  b=(ax > cx ? ax : cx);
  x=w=v=bx;
  fw=fv=fx=(*f)(x);
  for (iter=1;iter<=ITMAX;iter++) {
    xm=0.5*(a+b);
    tol2=2.0*(tol1=tol*fabs(x)+ZEPS);
    if (fabs(x-xm) <= (tol2-0.5*(b-a))) {
      *xmin=x;
      return fx;
    }
    if (fabs(e) > tol1) {
      r=(x-w)*(fx-fv);
      q=(x-v)*(fx-fw);
      p=(x-v)*q-(x-w)*r;
      q=2.0*(q-r);
      if (q > 0.0) p = -p;
      q=fabs(q);
      etemp=e;
      e=d;
      if (fabs(p) >= fabs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x))
	d=CGOLD*(e=(x >= xm ? a-x : b-x));
      else {
	d=p/q;
	u=x+d;
	if (u-a < tol2 || b-u < tol2)
	  d=SIGN(tol1,xm-x);
      }
    } else {
      d=CGOLD*(e=(x >= xm ? a-x : b-x));
    }
    u=(fabs(d) >= tol1 ? x+d : x+SIGN(tol1,d));
    fu=(*f)(u);
    if (fu <= fx) {
      if (u >= x) a=x; else b=x;
      SHFT(v,w,x,u)
	SHFT(fv,fw,fx,fu)
	} else {
	  if (u < x) a=u; else b=u;
	  if (fu <= fw || w == x) {
	    v=w;
	    w=u;
	    fv=fw;
	    fw=fu;
	  } else if (fu <= fv || v == x || v == w) {
	    v=u;
	    fv=fu;
	  }
	}
  }
  nrerror("Too many iterations in brent");
  *xmin=x;
  return fx;
}
#undef ITMAX
#undef CGOLD
#undef ZEPS
#undef SHFT
#undef NRANSI

#define TOL 2.0e-4

static int ncom;
static double *pcom = NULL, *xicom = NULL, (*nrfunc)(double []);

double f1dim (double x)
{
  static int j, ncom_max;
  static double f, *xt = NULL;

  if (ncom > ncom_max)
    {
      xt = cts_mrealloc (xt, double, ncom, "f1dim");
      ncom_max = ncom;
    }

  for (j=0; j < ncom; j++)
    xt[j] = pcom[j] + x * xicom[j];
  f=(*nrfunc)(xt);
  return f;
}


static void linmin(double p[], double xi[], int n, double *fret, double (*func)(double []))
{
	double brent(double ax, double bx, double cx,
		double (*f)(double), double tol, double *xmin);
	double f1dim(double x);
	void mnbrak(double *ax, double *bx, double *cx, double *fa, double *fb,
		double *fc, double (*func)(double));
	int j;
	static int n_max = 0;

	double xx,xmin,fx,fb,fa,bx,ax;

	if (n > n_max)
	  {
	    pcom = cts_mrealloc (pcom, double, n, "linmin");
	    xicom = cts_mrealloc (xicom, double, n, "linmin");
	    n_max = n;
	  }

	ncom=n;
	nrfunc=func;
	for (j=0;j<n;j++) {
		pcom[j]=p[j];
		xicom[j]=xi[j];
	}
	ax=0.0;
	xx=1.0;
	mnbrak(&ax,&xx,&bx,&fa,&fx,&fb,f1dim);
	*fret=brent(ax,xx,bx,f1dim,TOL,&xmin);
	for (j=0;j<n;j++) {
		xi[j] *= xmin;
		p[j] += xi[j];
	}
}
#undef TOL

#define NRANSI
#define ITMAX 2000
#define EPS 1.0e-10

void frprmn(double p[], int n, double ftol, int *iter, double *fret,
	double (*func)(double []), void (*dfunc)(double [], double []))
{
	static int n_max = 0;
	int j,its;
	double gg,gam,fp,dgg;
	static double *g = NULL, *h = NULL, *xi = NULL;

	if (n > n_max)
	  {
	    g = cts_mrealloc (g, double, n, "frprmn");
	    h = cts_mrealloc (h, double, n, "frprmn");
	    xi = cts_mrealloc (xi, double, n, "frprmn");
	    n_max = n;
	  }
	fp=(*func)(p);
	(*dfunc)(p,xi);
	for (j=0;j<n;j++) {
		g[j] = -xi[j];
		xi[j]=h[j]=g[j];
	}
	for (its=1;its<=ITMAX;its++) {
		*iter=its;
		linmin(p,xi,n,fret,func);
		if (2.0*fabs(*fret-fp) <= ftol*(fabs(*fret)+fabs(fp)+EPS)) {
			return;
		}
		fp=(*func)(p);
		(*dfunc)(p,xi);
		dgg=gg=0.0;
		for (j=0;j<n;j++) {
			gg += g[j]*g[j];
			dgg += (xi[j]+g[j])*xi[j];
		}
		if (gg == 0.0) {
			return;
		}
		gam=dgg/gg;
		for (j=0;j<n;j++) {
			g[j] = -xi[j];
			xi[j]=h[j]=g[j]+gam*h[j];
		}
	}
	nrerror("Too many iterations in frprmn");
}
#undef ITMAX
#undef EPS
#undef NRANSI


void gaussj (double **a, int n, double **b, int m)

  /* included bfirsttime check - D.K.
   */
{
  static bool bfirst_time = true;
  static int *indxc, *indxr, *ipiv;
  int i, icol, irow, j, k, l, ll;
  double big, dum, swap, pivinv;

  if (bfirst_time)
    {
      indxc = ivector (1, n);
      indxr = ivector (1, n);
      ipiv = ivector (1, n);

      bfirst_time = false; 
    }

  for (j = 1; j <= n; j++)
    ipiv[j] = 0;

  for (i = 1; i <= n; i++)
    {
      big = 0.0;
      for (j = 1; j <= n; j++)
	if (ipiv[j] != 1)
	  for (k = 1; k <= n; k++)
	    {
	      if (ipiv[k] == 0)
		{
		  if (fabs(a[j][k]) >= big)
		    {
		      big = fabs (a[j][k]);
		      irow = j;
		      icol = k;
		    }
		}
	      else
		if (ipiv[k] > 1)
		  nrerror ("gaussj: Singular Matrix-1");
	    }
      ++(ipiv[icol]);
      if (irow != icol)
	{
	  for (l = 1; l <= n; l++)
	    SWAP (a[irow][l], a[icol][l]);
	  for (l = 1; l <= m; l++)
	    SWAP (b[irow][l], b[icol][l]);
	}
      indxr[i] = irow;
      indxc[i] = icol;
      if (a[icol][icol] == 0.0)
	nrerror ("gaussj: Singular Matrix-2");
      pivinv = 1.0 / a[icol][icol];
      a[icol][icol] = 1.0;
      for (l = 1; l <= n; l++)
	a[icol][l] *= pivinv;
      for (l = 1; l <= m; l++)
	b[icol][l] *= pivinv;
      for (ll = 1; ll <= n; ll++)
	if (ll != icol)
	  {
	    dum=a[ll][icol];
	    a[ll][icol]=0.0;
	    for (l = 1; l <= n; l++)
	      a[ll][l] -= a[icol][l]*dum;
	    for (l = 1; l <= m; l++)
	      b[ll][l] -= b[icol][l]*dum;
	  }
    }
  for (l = n; l >= 1; l--)
    {
      if (indxr[l] != indxc[l])
	for (k = 1; k <= n; k++)
	  SWAP (a[k][indxr[l]], a[k][indxc[l]]);
    }
}


void covsrt (double **covar, int ma, int ia[], int mfit)

{
  int i, j, k;
  double swap;

  for (i = mfit + 1; i <= ma; i++)
    for (j = 1; j<= i; j++)
      covar[i][j] = covar[j][i] = 0.0;
  k = mfit;
  for (j = ma; j >= 1; j--)
    {
      if (ia[j])
	{
	  for (i = 1; i <= ma; i++)
	    SWAP (covar[i][k], covar[i][j]);
	  for (i = 1; i <= ma; i++)
	    SWAP (covar[k][i], covar[j][i]);
	  k--;
	}
    }
}



void lfit (double x[], double y[], double sig[], int ndat, double a[], int ia[],
	   int ma, double **covar, double *chisq, void (*funcs) ())

  /* included bfirsttime check - D.K.
   */
{
  static bool bfirst_time = true;

  int i, j, k, l, m, mfit = 0;
  double ym, wt,sum, sig2i;
  static double **beta, *afunc;

  if (bfirst_time)
    {
      beta = matrix (1, ma, 1, 1);
      afunc = vector (1, ma);

      bfirst_time = false; 
    }

  for (j = 1; j <= ma; j++)
    if (ia[j])
      mfit++;
  if (mfit == 0)
    nrerror ("lfit: no parameters to be fitted");
  for (j = 1; j <= mfit; j++)
    {
      for (k = 1; k <= mfit; k++)
	covar[j][k] = 0.0;
      beta[j][1] = 0.0;
    }
  for (i = 1; i <= ndat; i++)
    {
      funcs (x[i], afunc, ma);
      ym = y[i];
      if (mfit < ma)
	{
	  for (j = 1; j <= ma; j++)
	    if (!ia[j])
	      ym -= a[j] * afunc[j];
	}
      sig2i = 1.0 / m2 (sig[i]);
      for (j = 0, l = 1; l <= ma; l++)
	{
	  if (ia[l])
	    {
	      wt = afunc[l] * sig2i;
	      for (j++, k = 0, m = 1; m <= l; m++)
		if (ia[m])
		  covar[j][++k] += wt * afunc[m];
	      beta[j][1] += ym * wt;
	    }
	}
    }
  for (j = 2; j <= mfit; j++)
    for (k = 1; k < j; k++)
      covar[k][j]=covar[j][k];
  gaussj (covar, mfit, beta, 1);
  for (j = 0, l = 1; l <= ma; l++)
    if (ia[l]) a[l] = beta[++j][1];
  *chisq = 0.0;
  for (i = 1; i <= ndat; i++)
    {
      funcs (x[i], afunc, ma);
      for (sum = 0.0, j = 1; j <= ma; j++)
	sum += a[j] * afunc[j];
      *chisq += m2 ((y[i]-sum) / sig[i]);
	}
  covsrt (covar, ma, ia, mfit);
}

/* (C) Copr. 1986-92 Numerical Recipes Software 5.)-5K#R.. */
