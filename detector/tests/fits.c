#include <stdlib.h>
#include <stdio.h>
#include <math.h>

void main(int argc, char **argv)
{
  float index = -2.5;
  float llimit = 1.;
  float ulimit = 1000;
  float ll, ul, slex;
  float a, b, inc, xm;
  float x, y, lx, ly, f;
  int nbins = 30, n, maxn;
  int i, nums;
  int *h;
  float *lh, sumh, sumlh, hm, lhm;
  FILE *fin;

  llimit = atof(argv[1]);
  ulimit = atof(argv[2]);  
  nbins = atoi(argv[3]);
  index = atof(argv[4]);  
  nums = atoi(argv[5]);  
  srand48(atol(argv[6]));  

  ll = pow(llimit, index+1.0);
  ul = pow(ulimit, index+1.0);
  slex = 1.0 / (index+1.0);

  a = log10(llimit);
  b = log10(ulimit);
  inc = (b - a) / (float)nbins;

  h = (int*)malloc(nbins*sizeof(int));
  lh = (float*)malloc(nbins*sizeof(float));
  memset(h, '\0', nbins*sizeof(float));
  for (i=0; i<nbins; ++i) 
	lh[i] = 0.;

  for (n=0; n<nums; ++n) {

	f = (float) drand48();
	y = pow( f*ul + (1-f)*ll, slex);
	ly = log10( y );
	i = (int) floor( (ly - a) / inc );
	h[i]++;

  }

  maxn = nbins;
  for (i=0; i<nbins; ++i) 
	if (h[i] > 0) {
	  lh[i] = log10((float)h[i]);
	} else {
	  maxn = i+1;
	  i=nbins;
	}

  sumh = sumlh = 0.;
  for (i=0; i<maxn; ++i) {
	
	printf("%f %f %f\n", a+inc*.5+inc*i, (float)h[i], lh[i]);
	sumh += (float)h[i];
	sumlh += lh[i];

  }

  hm = sumh / (float)maxn;
  lhm = sumlh / (float)maxn;
  xm = (a+inc*maxn*0.5);

  sumh = sumlh = 0.;
  for (i=0; i<maxn; ++i) {
	
	sumh += (a+inc*.5+inc*i - xm)*(lh[i] - lhm);
	sumlh += (a+inc*.5+inc*i - xm)*(a+inc*.5+inc*i - xm);

  }

  fprintf(stderr, "%d %d %f %f %f %f ", 
		  nbins, maxn, a, b, a+inc*maxn, 
		  (float)maxn/(float)nbins);

  b = sumh / sumlh;
  a = lhm - b*xm;

  fprintf(stderr, "%f %f %.1f\n", a, b, (b/(index+1) - 1.)*100.);

}

	
