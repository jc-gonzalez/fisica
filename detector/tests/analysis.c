#include "analysis.h"

static float npix;                
static float m1x, m1y;            
static float m2xx, m2xy, m2yy;    
static float m2cxx, m2cxy, m2cyy; 
static float m3xxx, m3xxy, m3xyy, m3yyy;   
static float m3cxxx, m3cxxy, m3cxyy, m3cyyy;
static float charge;
static float xmax, ymax, smax;
static float xm, ym;
static float sx, sy;
static float sx2, sxy, sy2;
static float sx3, sx2y, sxy2, sy3;
static float rho;
static float length, width, dist, xdist, azw, miss, alpha, conc[9]; 
static float phi, asymx, asymy;
static float maxs[10];
static int nmaxs[10];

static float *q;

static float pixels[PIX_ARRAY_SIDE][PIX_ARRAY_SIDE][4];  
static float **pix; 
static int **pixneig; 
static int *npixneig; 

static float threshold = 0.0;  

static float plateScale;
static float ct_PixelWidth;
static float ct_PixelWidth_corner_2_corner;

const int MAX_NPIXELS = 1141;

void 
analysis( int n, float *image, int flag )
{
  register int i, j, k;

  float x, y;
  float x2m, xym, y2m;
  float x3m, x2ym, xy2m, y3m;
  float zz, zd, zu, zv;
  float ax, ay, unitx, unity, sigmaax;
  
  int (*compare) (const void *, const void *);

  if ( flag == 1 ) {
    q = (float*)calloc(n, sizeof(float));
    plateScale = (1.0/30.0);
    ct_PixelWidth = 3.0;
    ct_PixelWidth_corner_2_corner = ct_PixelWidth / cos(RAD(30.0));
    read_pixels(n);
    puts("DONE(3)!");
    return;
  } else {
    memcpy( q, image, sizeof(float) * n );
    for(i=0;i<n;++i)
      if (q[i]<threshold) q[i]=0.0;
  }

  /* save number of pixels */
  npix = n;
  
  /* calculate sum of values */
  xm = ym = 0.0;
  x2m = xym = y2m = 0.0;
  x3m = x2ym = xy2m = y3m = 0.0;
  charge = 0.0;
  
  for (i=0; i<npix; ++i)
    if ( q[i] > 0.0 ) {
    x = pix[i][0] * plateScale;
    y = pix[i][1] * plateScale;
    xm += x * q[i];
    ym += y * q[i];
    x2m += x * x * q[i];
    xym += x * y * q[i];
    y2m += y * y * q[i];
    x3m  += x * x * x * q[i];
    x2ym += x * x * y * q[i];
    xy2m += x * y * y * q[i];
    y3m  += y * y * y * q[i];
    charge += q[i];
  }

  /*++++++++++++++++++++++++++++++++++++++++++++++++++ 
   * extremes and charges
   *--------------------------------------------------*/

  for (i=0; i<10; ++i) 
    maxs[i] = 0.0;

  for (i=0; i<npix; ++i) {
    if ( q[i] > maxs[0] ) {
      for (k=9; k>0; --k)
        maxs[k] = maxs[k-1];
      for (k=9; k>0; --k)
        nmaxs[k] = nmaxs[k-1];
      maxs[0] = q[i];
      nmaxs[0] = i;
    }
  }

  /* calculates weighted position of the maximum (6 pixels) */

  xmax = ymax = smax = 0.0;

  for (i=0; i<6; ++i) {
    xmax += pix[nmaxs[i]][0] * q[nmaxs[i]];
    ymax += pix[nmaxs[i]][1] * q[nmaxs[i]];
    smax += q[nmaxs[i]];
  }

  if (smax > 0.0) {
    xmax /= smax;
    ymax /= smax;
  }

  /* calculate concentrations with 2,3,4...10 pixels */

  conc[0] = q[nmaxs[0]] + q[nmaxs[1]];

  for (i=2; i<10; ++i) 
    conc[i-1] = conc[i-2] + q[nmaxs[i]];

  if (charge > 0.0) 
    for (i=0; i<9; ++i) 
      conc[i] /= charge;  
  
  /*++++++++++++++++++++++++++++++++++++++++++++++++++ 
   * 1st moments
   *--------------------------------------------------*/

  if (charge > 0.0) {
    xm /= charge;
    ym /= charge;
  }

  m1x = xm;
  m1y = ym;

  /*++++++++++++++++++++++++++++++++++++++++++++++++++
   * 2nd moments
   *--------------------------------------------------*/

  if (charge > 0.0) {
    x2m /= charge;
    xym /= charge;
    y2m /= charge;
  }

  /* around the origin */
  m2xx = x2m;
  m2xy = xym;
  m2yy = y2m;
  
  /* around the mean */
  sx2 = x2m - SQR(xm);
  sxy = xym - xm*ym;
  sy2 = y2m - SQR(ym);

  m2cxx = sx2;
  m2cxy = sxy;
  m2cyy = sy2;

  /*++++++++++++++++++++++++++++++++++++++++++++++++++
   * 3rd moments
   *--------------------------------------------------*/

  if (charge > 0.0) {
    x3m  /= charge;
    x2ym /= charge;
    xy2m /= charge;
    y3m  /= charge;
  }

  /* around the origin */
  m3xxx = x3m;
  m3xxy = x2ym;
  m3xyy = xy2m;
  m3yyy = y3m;

  /* around the mean */
  sx3  = x3m  - 3*x2m*xm + 2*xm*xm*xm;
  sx2y = x2ym - 2*xym*xm + 2*xm*xm*ym - x2m*ym;
  sxy2 = xy2m - 2*xym*ym + 2*xm*ym*ym - y2m*xm;
  sy3  = y3m  - 3*y2m*ym + 2*ym*ym*ym;

  m3cxxx = x3m;
  m3cxxy = x2ym;
  m3cxyy = xy2m;
  m3cyyy = y3m;

  /*++++++++++++++++++++++++++++++++++++++++++++++++++ 
   * hillas' parameters
   *--------------------------------------------------*/

  zd = sy2 - sx2;
  zz = sqrt( SQR(zd) + 4.*SQR(sxy));;
  
  if ( zz < 1.e-6 ) 
    return;
  
  if (zz > 0.0) {

    zu = 1.0 + zd / zz;
    zv = 2.0 - zu;
    
    length = sqrt( fabs(sx2 + sy2 + zz)  /2. );
    width  = sqrt( fabs(sx2 + sy2 - zz) / 2. );
    dist   = sqrt( SQR(xm) + SQR(ym) );
    xdist  = sqrt( SQR(xmax) + SQR(ymax) );
    azw    = sqrt( fabs( SQR(xm)*y2m - 2.*xm*ym*xym + x2m*SQR(ym) ) );
    miss   = sqrt( fabs( (SQR(xm)*zu + SQR(ym)*zv)/2. - (2.*sxy*xm*ym/zz) ) );
    alpha  = DEG( asin(miss/dist) );

  } else {

    length = 0.0;
    width  = 0.0;
    dist   = 0.0;
    xdist  = 0.0;
    azw    = 0.0;
    miss   = 0.0;
    alpha  = 0.0;

  }

  /*++++++++++++++++++++++++++++++++++++++++++++++++++
   * asymetry
   *--------------------------------------------------*/
  
  unitx = sqrt(0.5*zv);
  unity = SGN(sxy)*sqrt(0.5*zu);

  if (xdist>0.0) {

    phi = acos((unitx*xmax+unity*ymax)/xdist);
  
    sigmaax = sx3*CUB(cos(phi)) + 3.0*sx2y*SQR(cos(phi))*sin(phi) +
      3.0*sxy2*cos(phi)*SQR(sin(phi)) + sy3*CUB(sin(phi));
    sigmaax = pow(fabs(sigmaax),0.3333333)*SGN(sigmaax);
    
    ax = sigmaax*unitx;
    ay = sigmaax*unity;
    asymx = (ax*xmax + ay*ymax)/(xdist*length*cos(phi));

  } else {

    phi=-1000.0;
    asymx = -1000.0;

  }
  
  return;
}

int
compare(const void *a, const void *b)
{ 
  static float a_, b_;
  a_ = *(float*)a;
  b_ = *(float*)b;
  if ( a_ < b_ ) 
    return ( -1 );
  if ( a_ > b_ ) 
    return ( +1 );
  return( 0 );
}

void
get_charge(float *c,float *x)
{ 
  *c = charge;
  *x = smax;
}

void
get_maxs(float *m, int *nm)
{ 
  memcpy( m, maxs, sizeof(float) * 10 );
  memcpy( nm, nmaxs, sizeof(int) * 10 );
}

void
get_hillas(float *len, float *wid, float *d, float *xd, 
           float *aw, float *mis, float *alph, float *cnc)
{ 
  *len = length;
  *wid = width;
  *d = dist;
  *xd = xdist;
  *aw = azw;
  *mis = miss;
  *alph = alpha;
  memcpy( cnc, conc, sizeof(float) * 9 );
}

void
get_asym(float *asx, float *asy, float *p)
{ 
  *asx = asymx;
  *asy = asymy;
  *p   = phi;
}

void 
set_threshold( float t )
{
  threshold = t;
}
     
void
read_pixels(int NPixels)
{
  FILE *pixfile, qefile;
  char line[200];
  int n, i, j;
  float x, y, qe;

  /*------------------------------------------------------------
   * first, pixels' coordinates
   */

  /* try to open the file */

  pixfile = fopen("pixels.dat","rt");
  
  /* initialize pixel numbers */

  for ( i=0; i<PIX_ARRAY_SIDE; ++i ) 
    for ( j=0; j<PIX_ARRAY_SIDE; ++j ) 
      pixels[i][j][PIXNUM] = -1;

  pix = (float**)calloc(NPixels,sizeof(float*));
  for ( i=0; i<NPixels; ++i ) 
    pix[i] = (float*)calloc(2,sizeof(float));

  /*
  pixneig = (int**)calloc(NPixels,sizeof(int*));
  for ( i=0; i<NPixels; ++i ) {
    pixneig[i] = (int*)calloc(2,sizeof(int));
    for ( j=0; j<6; ++j ) 
      pixneig[i][j] = -1;
  }

  npixneig = (int*)calloc(2,sizeof(int));
  for ( i=0; i<NPixels; ++i ) 
    npixneig[i] = 0;
  */
  /* read file */

  while ( fgets(line, 200, pixfile) != NULL ) {
    
    /* skip if comment */

    if ( *line == '#' )
      continue;

    /* get the value (dx, dy) */

    sscanf(line, "%d %d %d %f %f", &n, &i, &j, &x, &y);
    
    pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXNUM] = n-1;
    pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXX] = x;
    pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXY] = y;
   
    pix[n-1][0] = x;
    pix[n-1][1] = y;


    if (n==NPixels)
      break;

  }

  puts("DONE!");

  /* close file */
  
  fclose(pixfile);

  /* calculate tables of neighbours */
  
  /*
  for ( n=0 ; n<NPixels ; ++n )
    for ( i=n+1 ; (i<NPixels)&&(npixneig[n]<6) ; ++i) 
      if ( pixels_are_neig(n,i) ) {
        pixneig[n][npixneig[n]] = i;
        pixneig[i][npixneig[i]] = n;
        ++npixneig[n];
        ++npixneig[i];
      }
  */

}

int
pixels_are_neig(int pix1, int pix2)
{ 
  if ( sqrt(SQR( pix[pix1][0] - pix[pix2][0] ) +
            SQR( pix[pix1][1] - pix[pix2][1] ) ) 
       > ct_PixelWidth_corner_2_corner ) 
    return ( FALSE );
  else
    return ( TRUE );
}
