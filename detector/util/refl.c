#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <sys/types.h>
#include <dirent.h>

#include "jcmacros.h"

#define RandomNumber  drand48()

#define NORM(v) (sqrt( SQR(v[0]) + SQR(v[1]) + SQR(v[2]) ))
#define RAD(x)  ((x)*0.0174532925199433)
#define DEG(x)  ((x)*57.2957795130823)

float Speed_of_Light_vacuum;
float Speed_of_Light_air;
float Speed_of_Light_vacuum_cmns;
float Speed_of_Light_air_cmns;

#define CT_ITEM_LIST \
T(type),            \
T(focal_distance),  \
T(focal_std),       \
T(point_spread),    \
T(point_std),       \
T(black_spot),      \
T(n_mirrors),       \
T(r_mirror),        \
T(camera_width),    \
T(n_pixels),        \
T(pixel_width),     \
T(define_mirrors)   
  
#define T(x)  x    
  
  enum CT_ITEM_TYPE {
    CT_ITEM_LIST
  };

#undef T

#define T(x)  #x   

const char *const CT_ITEM_NAMES[] = {
  CT_ITEM_LIST
};

#undef T

#define CT_I       0

#define CT_S       1
#define CT_RHO     2
#define CT_THETA   3

#define CT_FOCAL   1
#define CT_SX      2
#define CT_SY      3

#define CT_X       4
#define CT_Y       5
#define CT_Z       6
#define CT_THETAN  7
#define CT_PHIN    8
#define CT_XN      9
#define CT_YN     10
#define CT_ZN     11

#define CT_NDATA  12

#define LINE_MAX_LENGTH   200
#define ITEM_MAX_LENGTH   40
 
void makeOmega(float theta, float phi);
void makeOmegaI(float theta, float phi);
void applyMxV(float M[3][3], float *V, float *Vp);
float dist_r_P(float a, float b, float c, 
               float u, float v, float w,
               float x, float y, float z);
float Curv2Lin(float s);
float Lin2Curv(float x);

static float OmegaCT[3][3];
static float OmegaICT[3][3];

static float Omega[3][3];   
static float OmegaI[3][3];  

static int   ct_Type;       
static float ct_Focal[1000];  
static float ct_Focal_mean;  
static float ct_Focal_std;   
static float ct_PSpread_mean;
static float ct_PSpread_std; 
static float ct_BlackSpot_rad;
static float ct_RMirror;      
static float ct_CameraWidth;  
static float ct_PixelWidth;   
static int ct_NMirrors = 0;   
static int ct_NPixels;        
static float ct_data[1000][12];       

int
main(int argc, char **argv) 
{

  char cername[256];
  char outname[256];
  char ctname[256];

  FILE *fin, *fout;

  float thetaCT, phiCT, xiCT; 
  float coreD, coreX, coreY;  
  float u, v, w;              
			      
  float r[3];                 
  float x[3];                 
			      
  float rCT[3];               
  float xCT[3];               
                              
  float rm[3];                
  float xmm[3];               
  float xm[3];                
                              
  float xcut[3];              
  float xcutCT[3];            
                              
  float rnor[3], rnorm;       
			      
  float rrefl[3];             
  float rreflCT[3];           
  float xcam[3];              
			      
  float calpha;               
  float phi;                  
			      
  float a, b, c, t;           
			      
  float mind, d;              
			      
  float wl;                   
  float reflec;               
			      
  int ncer;                   
  int np, nf;                 
  int i, j, k, n;             
  int i_mirror=-1;            
			      
  int nCphotons;              
			      
  float TR;                   
  int nbeforeTR, nafterTR;    
  int num_cer_files;          
  int max_num_cer_files;      
			      
  float lE, uE;               

  float distmirr, distmirr2;  
  float sx, sy;
  float t1, t2;

  char line[LINE_MAX_LENGTH]; 
  char token[ITEM_MAX_LENGTH];
  char fmirr[40];             

  int is_end = FALSE;

  Speed_of_Light_vacuum = 299792458.0; 
  Speed_of_Light_air = Speed_of_Light_vacuum / 1.000293;
  
  Speed_of_Light_vacuum_cmns = Speed_of_Light_vacuum / 1.0e7;
  Speed_of_Light_air_cmns = Speed_of_Light_air / 1.0e7;
  
  strcpy(cername, argv[1]);
  strcpy(outname, argv[2]);
  strcpy(ctname, argv[3]);

  fin = fopen( ctname, "rt" ); 

  while ( !feof(fin) ) {          

    fgets( line, LINE_MAX_LENGTH, fin );

    for (i=0; i<=define_mirrors; i++) 
      if (strstr(line, CT_ITEM_NAMES[i]) == line)
        break;
    
    if (i == define_mirrors+1) 
      continue;
    
    switch ( i ) {

    case type:    
      
      sscanf(line, "%s %d", token, &ct_Type);

      break;

    case focal_distance:  

      sscanf(line, "%s %f", token, &ct_Focal_mean);

      break;

    case focal_std:       

      sscanf(line, "%s %f", token, &ct_Focal_std);

      break;

    case point_spread:    

      sscanf(line, "%s %f", token, &ct_PSpread_mean);

      break;

    case point_std:       

      sscanf(line, "%s %f", token, &ct_PSpread_std);

      break;

    case black_spot:      

      sscanf(line, "%s %f", token, &ct_BlackSpot_rad);

      break;

    case r_mirror:        

      sscanf(line, "%s %f", token, &ct_RMirror);

      break;

    case n_mirrors:       

      sscanf(line, "%s %d", token, &ct_NMirrors);

      break;

    case camera_width:    

      sscanf(line, "%s %f", token, &ct_CameraWidth);

      break;

    case n_pixels:        

      sscanf(line, "%s %d", token, &ct_NPixels);

      break;

    case pixel_width:     

      sscanf(line, "%s %f", token, &ct_PixelWidth);

      break;

    case define_mirrors:  

      for (i=0; i<ct_NMirrors; i++) {

	fgets( line, LINE_MAX_LENGTH, fin );
	sscanf(line, "%f %f %f %f %f %f %f %f %f %f %f %f",
	       &ct_data[i][0],
	       &ct_data[i][1],
	       &ct_data[i][2],
	       &ct_data[i][3],
	       &ct_data[i][4],
	       &ct_data[i][5],
	       &ct_data[i][6],
	       &ct_data[i][7],
	       &ct_data[i][8],
	       &ct_data[i][9],
	       &ct_data[i][10],
	       &ct_data[i][11]);
	
      }

      break;
      
    } 

  } 

  fclose(fin);

  fin = fopen( cername, "rt" );
  
  fscanf( fin, "%f %f %f %f", &coreX, &coreY, &thetaCT, &phiCT );

  fprintf(stderr, "core: %f %f      dir: %f %f\n", 
	  coreX, coreY, thetaCT, phiCT);
  makeOmega( thetaCT, phiCT );
  makeOmegaI( thetaCT, phiCT );

  memcpy( OmegaCT, Omega, 9*sizeof(float) );
  memcpy( OmegaICT, OmegaI, 9*sizeof(float) );

  while ( ! feof(fin) ) {

    fscanf( fin, "%f %f %f %f %f %f", 
	    &x[0], &x[1], &x[2], &r[0], &r[1], &r[2] );

    x[0] -= coreX;
    x[1] -= coreY;

    applyMxV( OmegaCT, x, xCT);
    applyMxV( OmegaCT, r, rCT);        

    /*
     * In[1]:= esfera:=x^2+y^2+(z-R)^2-R^2;
     *         recta:={x->x0+u/w(z-z0),y->y0+v/w(z-z0)}
     * 
     * In[2]:= esfera
     * 
     *            2    2    2           2
     * Out[2]= -R  + x  + y  + (-R + z)
     * 
     * In[3]:= recta
     * 
     *                     u (z - z0)            v (z - z0)
     * Out[3]= {x -> x0 + ----------, y -> y0 + ----------}
     *                         w                     w
     * 
     * In[4]:= esf=esfera /. recta
     * 
     *           2           2         u (z - z0) 2         v (z - z0) 2
     * Out[4]= -R  + (-R + z)  + (x0 + ----------)  + (y0 + ----------)
     *                                      w                    w
     * 
     * In[5]:= coefs=CoefficientList[ExpandAll[esf],z]
     * 
     *                                               2   2    2   2
     *            2     2   2 u x0 z0   2 v y0 z0   u  z0    v  z0
     * Out[5]= {x0  + y0  - --------- - --------- + ------ + ------, 
     *                           w           w          2        2
     *                                                 w        w
     *  
     *                                  2         2          2    2
     *             2 u x0   2 v y0   2 u  z0   2 v  z0      u    v
     * >    -2 R + ------ + ------ - ------- - -------, 1 + -- + --}
     *               w        w         2         2          2    2
     *                                 w         w          w    w
     */
    
    a = SQR(rCT[0]) + SQR(rCT[1]) + SQR(rCT[2]);
    b = (-2*(2.*ct_Focal_mean*SQR(rCT[2]) - rCT[0]*rCT[2]*xCT[0] 
	     + SQR(rCT[0])*xCT[2] 
	     + rCT[1]*(-(rCT[2]*xCT[1]) + rCT[1]*xCT[2])));
    c = (SQR(rCT[2])*(SQR(xCT[0]) + SQR(xCT[1])) 
	 - 2*rCT[2]*(rCT[0]*xCT[0] + rCT[1]*xCT[1])*xCT[2] + 
	 (SQR(rCT[0]) + SQR(rCT[1]))*SQR(xCT[2]));
	
    d = sqrt( b*b - 4.0*a*c );

    t1 = (-b+d) / (2.0*a);
    t2 = (-b-d) / (2.0*a);

    xcut[2] = (t1 < t2) ? t1 : t2;
    xcut[0] = xCT[0] + rCT[0]/rCT[2]*(xcut[2]-xCT[2]);
    xcut[1] = xCT[1] + rCT[1]/rCT[2]*(xcut[2]-xCT[2]);

    if ( ct_Type == 1 ) {

      sx = Lin2Curv( xcut[0] );
      sy = Lin2Curv( xcut[1] );
	  
      if ((fabs(sx) > 850.0) || (fabs(sy) > 850.0)) {
	continue;
      }

    } 

    distmirr = 1000000.;
    
    for (i=0; i<ct_NMirrors && distmirr>=ct_RMirror; ++i) {
      distmirr2 = sqrt(SQR(ct_data[i][CT_X] - xcut[0]) +
		       SQR(ct_data[i][CT_Y] - xcut[1]) +
		       SQR(ct_data[i][CT_Z] - xcut[2]));
      if (distmirr2 < distmirr) {
	i_mirror = i;
	distmirr = distmirr2;
      }
    }
	/*
          cout << "#M"
               << ' ' << x[0]
               << ' ' << x[1]
               << ' ' << x[2]
	       << ' ' << distmirr 
	       << ' ' << i_mirror
	       << endl << flush;
	/*
          cout << '#' << t   
               << ' ' << x[0]
               << ' ' << x[1]
               << ' ' << x[2]
               << ' ' << r[0]
               << ' ' << r[1]
               << ' ' << r[2] 
               << ' ' << thetaCT 
               << ' ' << phiCT 
               << ' ' << xCT[0]
               << ' ' << xCT[1]
               << ' ' << xCT[2]
               << ' ' << rCT[0]
               << ' ' << rCT[1]
               << ' ' << rCT[2]
               << ' ' << xcut[0]
               << ' ' << xcut[1]
               << ' ' << xcut[2]
               << ' ' << sx
               << ' ' << sy
               << ' ' << i_mirror
               << ' ' << ct_data[i_mirror][CT_SX]
               << ' ' << ct_data[i_mirror][CT_SY]
               << ' ' << ct_data[i_mirror][CT_SX] - sx
               << ' ' << ct_data[i_mirror][CT_SY] - sy
               << endl << flush;
          */

    if ( ct_Type == 0 ) {

      if (distmirr > ct_RMirror) {
	continue;
      }
	
    } else {
	  
      if ((fabs(ct_data[i_mirror][CT_SX] - sx) > ct_RMirror) ||
	  (fabs(ct_data[i_mirror][CT_SY] - sy) > ct_RMirror)) {
	continue;	    
      }
      
    }

    makeOmega (-ct_data[i_mirror][CT_THETAN], 
	       ct_data[i_mirror][CT_PHIN]);
    makeOmegaI(-ct_data[i_mirror][CT_THETAN], 
	       ct_data[i_mirror][CT_PHIN]);
        
    xmm[0] = xCT[0] - ct_data[i_mirror][CT_X];
    xmm[1] = xCT[1] - ct_data[i_mirror][CT_Y];
    xmm[2] = xCT[2] - ct_data[i_mirror][CT_Z];
        
    applyMxV( Omega, xmm, xm );
    applyMxV( Omega, rCT, rm );
        
    a = 1.0;
    b = 2.0*(xm[0]*rm[0] + 
	     xm[1]*rm[1] + 
	     xm[2]*rm[2] - 2.0*ct_Focal[i_mirror]*rm[2]);
    c = (SQR(xm[0]) + 
	 SQR(xm[1]) + 
	 SQR(xm[2]) - 4.0*ct_Focal[i_mirror]*xm[2]);

    t = ( -b - sqrt( b*b - 4.0*a*c ) ) / ( 2*a );
        
    xcut[0] = xm[0] + rm[0]*t;
    xcut[1] = xm[1] + rm[1]*t;
    xcut[2] = xm[2] + rm[2]*t;
        
    if ( sqrt(SQR(xcut[0]) + SQR(xcut[1])) < ct_BlackSpot_rad ) {
      continue;
    }

    rnor[0] = 2.0*xcut[0];
    rnor[1] = 2.0*xcut[1];
    rnor[2] = -2.0*(2.0*ct_Focal[i_mirror] - xcut[2]);
    
    rnorm = -NORM( rnor );
    rnor[0] /= rnorm;
    rnor[1] /= rnorm;
    rnor[2] /= rnorm;
    
    calpha = fabs(rnor[0]*rm[0] + 
		  rnor[1]*rm[1] + 
		  rnor[2]*rm[2]);
        
    rrefl[0] = 2.0*rnor[0]*calpha - rm[0];
    rrefl[1] = 2.0*rnor[1]*calpha - rm[1];
    rrefl[2] = 2.0*rnor[2]*calpha - rm[2];
    
    rnorm = NORM( rrefl );
    rrefl[0] /= rnorm;
    rrefl[1] /= rnorm;
    rrefl[2] /= rnorm;
    
    applyMxV( OmegaI, xcut, xcutCT);
    applyMxV( OmegaI, rrefl, rreflCT);
    
    xcutCT[0] += ct_data[i_mirror][CT_X];
    xcutCT[1] += ct_data[i_mirror][CT_Y];
    xcutCT[2] += ct_data[i_mirror][CT_Z];
    
    t = (ct_Focal_mean - xcutCT[2]) / rreflCT[2];
    
    xcam[0] = xcutCT[0] + rreflCT[0]*t;
    xcam[1] = xcutCT[1] + rreflCT[1]*t;
    xcam[2] = xcutCT[2] + rreflCT[2]*t;
    
    if ( (SQR(xcam[0])+SQR(xcam[1])) > SQR(ct_CameraWidth) ) {
      continue;
    }
    
    phi = asin(rreflCT[2]);  
    
    t = - sqrt( SQR(xm[0] - xcut[0]) +
                  SQR(xm[1] - xcut[1]) +
                  SQR(xm[2] - xcut[2]) ) / Speed_of_Light_air_cmns;
    
    t = t + sqrt( SQR(xcutCT[0] - xcam[0]) +
                  SQR(xcutCT[1] - xcam[1]) +
                  SQR(xcutCT[2] - xcam[2]) ) / Speed_of_Light_air_cmns;

    printf("%e %e %e %e %e %e %e %e %e %e %e %e %e\n",
           x[0], x[1], x[2], 
	   r[0], r[1], r[2], 
	   xcam[0], xcam[1], xcam[2], 	   
	   i_mirror, distmirr,
	   phi, t);

  }

  fclose( fin );

  return ( 0 );
}


void makeOmega (float theta, float phi)
{
  static float ct, st, cp, sp;
  
  ct = cos(theta);
  st = sin(theta);
  cp = cos(phi);
  sp = sin(phi);
  
  Omega[0][0] =  cp*ct;
  Omega[0][1] =  sp*st; 
  Omega[0][2] = -st; 
     
  Omega[1][0] = -sp;
  Omega[1][1] =  cp;
  Omega[1][2] =  0;      

  Omega[2][0] =  cp*st;
  Omega[2][1] =  sp*st; 
  Omega[2][2] =  ct;         
}

void makeOmegaI(float theta, float phi)
{
  static float ct, st, cp, sp;
  
  ct = cos(theta);
  st = sin(theta);
  cp = cos(phi);
  sp = sin(phi);
  
  OmegaI[0][0] =  cp*ct;
  OmegaI[0][1] = -sp;
  OmegaI[0][2] =  cp*st;     

  OmegaI[1][0] =  sp*ct;
  OmegaI[1][1] =  cp; 
  OmegaI[1][2] =  sp*st;

  OmegaI[2][0] = -st;   
  OmegaI[2][1] =  0; 
  OmegaI[2][2] =  ct;            
}

void applyMxV(float M[3][3], float *V, float *Vp)
{
  Vp[0] = (M[0][0] * V[0] +
           M[0][1] * V[1] +
           M[0][2] * V[2]);
  Vp[1] = (M[1][0] * V[0] +
           M[1][1] * V[1] +
           M[1][2] * V[2]);
  Vp[2] = (M[2][0] * V[0] +
           M[2][1] * V[1] +
           M[2][2] * V[2]);
}

float  dist_r_P(float a, float b, float c, 
         float l, float m, float n,
         float x, float y, float z)
{
  return (
          sqrt((SQR((a-x)*m-(b-y)*l) +
                SQR((b-y)*n-(c-z)*m) +
                SQR((c-z)*l-(a-x)*n))/
               (SQR(l)+SQR(m)+SQR(n))
               )
          );
}

float  Curv2Lin(float s)
{
  float x;
  short i;

  x = s;
  for (i = 0; i < 4; i++)
    x = (s / 100.) / (1. + (float) 0.000144175317185 * x * x);

  return (x*100.);
}

float Lin2Curv(float x)
{
  x /= 100.;
  return ((x + (float) 0.000144175317185 * x * x * x)*100.);
}
