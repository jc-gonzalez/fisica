#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "jcmacros.h"

//-- random numbers stuff
extern "C" { 
#include "ranlib.h"
}
#define RandomNumber ranf()

template<class T>
T sqr(T x){ return x*x; }

template<class T>
void swap(T &x, T &y)
{
  T temp = sx;
  sx = sy;
  sy = temp;
}

// number of bins in X and Y direction

static int XBins;
static int YBins;
  
//-- lower and upper edges in both X and Y direction

const double xrange1 = -10.;
const double xrange2 =  10.;

const double yrange1 = -10.;
const double yrange2 =  10.;

const double xrange = xrange2 - xrange1;
const double yrange = yrange2 - yrange1;

//-- bin sizes in each direction

double deltax;
double deltay;

//-- image in the camera (square pixels to do it faster/easier)

double **image;
double **devs;

typedef struct {

  // moments
  double m1x, m1y;                       // first moments (mean)
  double m2xx, m2xy, m2yy;               // second moments (around origin)
  double m2cxx, m2cxy, m2cyy;            // second moments (around mean)
  double m3xxx, m3xxy, m3xyy, m3yyy;     // third moments (around origin)
  double m3cxxx, m3cxxy, m3cxyy, m3cyyy; // third moments (around mean)

  // charges
  double charge;                         // total charge in the image
  double xmax, ymax;                     // position of the maximum
  double smax;                           // charge of the block of maximum
  double maxs[10];                       // charges of the first 10 max.
  int nmaxs[10];                        // number of pixels of 10 max.
  
  // parameters of the image
  double length, width, dist, xdist, azw, miss, alpha, conc[9], beta; 
  double phi, asymx, asymy;
  
} Moments_Info;

Moments_Info m;

//-- prototypes

inline void clean_image();
inline void dump_image();
inline void dump_image_gnuplot();
inline void show_userpar();
inline void show_imgpar();
inline void show_ntdata();
static void put_image();
static void put_star(double ox, double oy, double s, double intensity);
static void calc_imgpar();

double ox;
double oy;
double sx;
double sy;
double amp;
double rho;
double corho2;
double sx2, sy2, sxsy;
double theta;
double rv, ru;

double a, b;
  
double ctheta, stheta, s2theta;
double ctheta2, stheta2;
  
bool cleanStar = false;

int main(int argc, char **argv)
{
  double starfactor;
  bool putStar = false;
  
  XBins = atoi(argv[1]);
  YBins = atoi(argv[1]);
  
  int nimages = atoi(argv[2]);

  starfactor = (double)atof(argv[3]);
  putStar = (starfactor!=0.0) ? true : false;
  cleanStar = (starfactor<0.0) ? true : false;
  starfactor = fabs(starfactor);
  
  //-- bin sizes in each direction
 
  deltax = xrange / XBins;
  deltay = yrange / YBins;

  int i;

  double r;

  image = new double * [XBins];
  for (i=0; i<XBins; i++)
    image[i] = new double [YBins];

  devs = new double * [XBins];
  for (i=0; i<XBins; i++)
    devs[i] = new double [YBins];

  time_t tseconds;

  time( &tseconds );

  setall((long)tseconds, (long)tseconds+1);
  
  for (i=0; i<nimages; i++) {

    clean_image();

    r = -1.0;
    
    while ((r<0.5) || (r>8.0)) {
      ox = RandomNumber * 16.0 - 8.0;
      oy = RandomNumber * 16.0 - 8.0;
      r = sqrt(sqr(ox)+sqr(oy));
    }
    
    amp = RandomNumber * 90.0 + 10.0;

    sx = RandomNumber * 1.0 + 0.5;
    sy = RandomNumber * 1.0 + 0.5;
    
    theta = RandomNumber * 2.0 * M_PI;

    sx2 = sqr(sx);
    sy2 = sqr(sy);
    sxsy = sx*sy;

    put_image();
    if (putStar) put_star(5., 5., .2, starfactor*amp);

    calc_imgpar();

    theta = atan(tan(theta));
    if (sx < sy) swap(sx, sy);
    rho = tan(2.0*theta) * ((sx2-sy2) / (2.0*sxsy));

    // show_userpar();
    // show_imgpar();

    show_ntdata();
    
    // dump_image();
    // dump_image_gnuplot();
    
  }

  return 0;
}

//++
//  puts to 0 all the cells in the image array
//--
inline void clean_image()
{
  register int i;
  register int j;
  for (i=0; i<XBins; i++)
    for (j=0; j<YBins; j++)
      image[i][j] = 0.0;
}

//++
//  writes the image to the stdout
//--
inline void dump_image()
{
  register int i;
  register int j;
  for (i=0; i<XBins; i++) {
    for (j=0; j<YBins; j++)
      cout << image[i][j] << endl;
    //cout << endl << flush;
  }
}

//++
//  writes the image to the stdout in gnuplot format
//--
inline void dump_image_gnuplot()
{
  register int i;
  register int j;
  for (i=0; i<XBins; i++) {
    for (j=0; j<YBins; j++)
      cout << image[i][j] << endl;
    cout << endl << flush;
  }
}

//++
//  fills the image array
//--
static void put_image()
{
  static double factor;
  static double f;

  static double x;
  static double y;

  static double rx;
  static double ry;

  ctheta = cos(theta);
  stheta = sin(theta);
  
  factor = amp / (2 * M_PI * sxsy );
  f = -0.5;

  register int i;
  register int j;
 
  x = xrange1 + deltax/2. - ox;

  for (i=0; i<XBins; i++) {

    x += deltax;

    y = yrange1 + deltay/2. - oy;
    
    for (j=0; j<YBins; j++) {

      y += deltay;

      rx =   x * ctheta + y * stheta;
      ry = - x * stheta + y * ctheta;
      
      image[i][j] = factor * exp( f * ( (sqr(rx)/sx2) + (sqr(ry)/sy2) ) );

      devs[i][j] = 1.0;
    }
    
  }

}

//++
//  adds an star to the image array
//--
static void put_star(double ox, double oy, double s, double intensity)
{
  static double factor;
  static double f;

  static double x;
  static double y;

  static double s2;

  factor = intensity / (2 * M_PI * sqr(s) );
  f = -0.5;

  s2 = sqr(s);

  register int i;
  register int j;
 
  x = xrange1 + deltax/2. - ox;

  for (i=0; i<XBins; i++) {

    x += deltax;

    y = yrange1 + deltay/2. - oy;
    
    for (j=0; j<YBins; j++) {

      y += deltay;

      image[i][j] += factor * exp( f * ( (sqr(x)+ sqr(y))/s2 ) );

      if (cleanStar)
        devs[i][j] += 1000.0;
    }
    
  }

}

inline void show_userpar()
{
  cerr << "# " << ox << ' ' << oy << ' ' << sx << ' ' << sy << ' '
       << amp << ' ' << rho << ' ' << endl;
}

inline void show_imgpar()
{
  cerr << "(X,Y) = (" << m.m1x << "," << m.m1y << ")" << endl;
  cerr << "ALPHA  = " << m.alpha  << endl;
  cerr << "LENGTH = " << m.length << endl;
  cerr << "WIDTH  = " << m.width  << endl;
  cerr << "DIST   = " << m.dist   << endl;
  cerr << "ASYMX  = " << m.asymx  << endl;
  cerr << "BETA   = " << m.beta   << endl;
  cerr << flush;
}

inline void show_ntdata()
{
  cout << ox << ' ' << oy << ' ' 
       << sx << ' ' << sy << ' '
       << amp << ' ' << rho << ' '
       << theta << ' '
       << rv << ' ' << ru << ' ';
  cout << m.m1x    << ' ' << m.m1y    << ' '
       << m.alpha  << ' '
       << m.length << ' ' << m.width  << ' '
       << m.dist   << ' '
       << m.asymx  << ' ' << m.beta
       << endl << flush;
}

static void calc_imgpar()
{
  register int i;
  register int j;

  static double x, y;
  static double xm, ym;
  static double x2m, xym, y2m;
  static double x3m, x2ym, xy2m, y3m;
  static double zz, zd, zu, zv;
  static double ax, ay, unitx, unity, sigmaax;
  static double sx2, sxy, sy2;
  static double sx3, sx2y, sxy2, sy3;
  static double w;
  
  // calculate sum of values
  xm = ym = 0.0;
  x2m = xym = y2m = 0.0;
  x3m = x2ym = xy2m = y3m = 0.0;
  m.charge = 0.0;
  
  x = xrange1 + deltax/2.;

  for (i=0; i<XBins; i++) {

    x += deltax;

    y = yrange1 + deltay/2.;
    
    for (j=0; j<YBins; j++) {

      y += deltay;

      w = image[i][j] * ( 1.0 / sqr(devs[i][j]) );
      
      xm += x * w;
      ym += y * w;
      x2m += x * x * w;
      xym += x * y * w;
      y2m += y * y * w;
      x3m  += x * x * x * w;
      x2ym += x * x * y * w;
      xy2m += x * y * y * w; 
      y3m  += y * y * y * w;

      m.charge += w;
    }

  }
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++ 
  // extremes and charges
  //--------------------------------------------------

  double &maximum = m.maxs[0];

  maximum = -1.0;
  
  for (i=0; i<XBins; i++) {
    for (j=0; j<YBins; j++) {
      if (image[i][j] > maximum) {
        maximum = image[i][j];
        m.xmax = xrange1 + (i+0.5)*deltax;
        m.ymax = yrange1 + (j+0.5)*deltay;
      }
    }
  }
      
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++ 
  // 1st moments
  //--------------------------------------------------

  xm /= m.charge;
  ym /= m.charge;

  m.m1x = xm;
  m.m1y = ym;

  
  //++++++++++++++++++++++++++++++++++++++++++++++++++
  // 2nd moments
  //--------------------------------------------------

  x2m /= m.charge;
  xym /= m.charge;
  y2m /= m.charge;

  // around the origin
  m.m2xx = x2m;
  m.m2xy = xym;
  m.m2yy = y2m;
  
  // around the mean
  sx2 = x2m - SQR(xm);
  sxy = xym - xm * ym;
  sy2 = y2m - SQR(ym);

  m.m2cxx = sx2;
  m.m2cxy = sxy;
  m.m2cyy = sy2;

  
  //++++++++++++++++++++++++++++++++++++++++++++++++++
  // 3rd moments
  //--------------------------------------------------

  x3m  /= m.charge;
  x2ym /= m.charge;
  xy2m /= m.charge;
  y3m  /= m.charge;

  // around the origin
  m.m3xxx = x3m;
  m.m3xxy = x2ym;
  m.m3xyy = xy2m;
  m.m3yyy = y3m;

  // around the mean
  sx3  = x3m  - 3 * x2m * xm + 2 * xm * xm * xm;
  sx2y = x2ym - 2 * xym * xm + 2 * xm * xm * ym - x2m * ym;
  sxy2 = xy2m - 2 * xym * ym + 2 * xm * ym * ym - y2m * xm;
  sy3  = y3m  - 3 * y2m * ym + 2 * ym * ym * ym;

  m.m3cxxx = x3m;
  m.m3cxxy = x2ym;
  m.m3cxyy = xy2m;
  m.m3cyyy = y3m;

  
  //++++++++++++++++++++++++++++++++++++++++++++++++++ 
  // hillas' parameters
  //--------------------------------------------------

  zd = sy2 - sx2;
  zz = sqrt( SQR(zd) + 4.*SQR( sxy ));;
  if ( (zz < 1.e-6) || (sxy == 0.) ) {
    m.dist = -1.;
  }
  zu = 1.0 + zd / zz;
  zv = 2.0 - zu;

  
  m.length = sqrt( fabs(sx2 + sy2 + zz) / 2. );
  m.width  = sqrt( fabs(sx2 + sy2 - zz) / 2. );
  m.dist   = sqrt( SQR(xm) + SQR(ym) );
  m.xdist  = sqrt( SQR(m.xmax) + SQR(m.ymax) );
  m.azw    = sqrt( fabs( SQR(xm)*y2m - 2.*xm*ym*xym + x2m*SQR(ym) ) ) / m.dist;
  m.miss   = sqrt( fabs( (SQR(xm)*zu + SQR(ym)*zv)/2. - (2.*sxy*xm*ym/zz) ) );
  m.alpha  = DEG( asin( m.miss/m.dist ) );
  m.beta   = atan(zz / ( 2. * sxy ));
 

  //++++++++++++++++++++++++++++++++++++++++++++++++++
  // asymetry
  //--------------------------------------------------
  
  unitx = sqrt(0.5*zv);
  unity = SGN( sxy )*sqrt(0.5*zu);

  if ( m.xdist > 0.0 ) {

    m.phi = acos((unitx*m.xmax + unity*m.ymax )/m.xdist);
  
    sigmaax = sx3*CUB(cos(m.phi)) + 3.0*sx2y*SQR(cos(m.phi))*sin(m.phi) +
      3.0*sxy2*cos(m.phi)*SQR(sin(m.phi)) + sy3*CUB(sin(m.phi));
    sigmaax = pow(fabs(sigmaax),0.3333333)*SGN(sigmaax);
    
    ax = sigmaax*unitx;
    ay = sigmaax*unity;
    m.asymx = (ax*m.xmax + ay*m.ymax)/(m.xdist*m.length*cos(m.phi));
    m.asymy = 0.0;

  } else {

    m.phi=-1000.0;
    m.asymx = -1000.0;
    m.asymy = -1000.0;

  }

}
