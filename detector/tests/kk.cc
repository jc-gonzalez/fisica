#include <iostream.h>
#include <math.h>

#include <stdio.h>

inline long num_pixels( long r )
{
  return ( 3 * r * (r + 1) + 1 );
}

static double fact( int m )
{
  return ( (m < 2) ? 1.0 : m * fact(m - 1) );
}

static double combinatorial( int m, int n )
{
  double c=1.0;
  int i;
  long a;
  for (i=0, a=m; i<n; i++, a--) { c *= a; } 
  for (i=0, a=n; i<n; i++, a--) { c /= a; } 
  return ( c );
}

int main()
{
  // number of photoelectrons in threshold
  int   q0[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int   iq0 = sizeof(q0) / sizeof(q0[0]);
  
  // trigger pattern (>0 SMn, <0 NNn)
  int   trigger[] = {2, 3, 4, 5, -2, -3, -4};
  int   itrigger = sizeof(trigger) / sizeof(trigger[0]);
  
  // time window (ns)
  int   time[] = {3, 4, 5, 6, 8, 10, 15};
  int   itime = sizeof(time) / sizeof(time[0]);
  
  // number of ring
  int   rings[] = {4, 6, 8, 10};
  int   irings = sizeof(rings) / sizeof(rings[0]);
  
  // different PMT QEs
  float qe[] = {17.0, 45.0, 80.0};
  int   iqe = sizeof(qe) / sizeof(qe[0]);
  
  int i, j, k, l, m, n;

  int t;  
  long  npixels;
  float ftime;
  unsigned long  ncomb;
  float LONS_mean_pmts = 0.11;
  float LONS_mean;
  float LONS;
  int   rq0;
  float prob;
  float rate;
  float atr;
  bool sm;
  char prefix;
  
  // we assume QE

  for ( m=0; m<iqe; m++) {

    LONS_mean = LONS_mean_pmts * qe[m] / qe[0];

    for ( i=0; i<irings; i++ ) {
      
      npixels = num_pixels( rings[i] );
      
      for ( j=0; j<itime; j++ ) {
        
        ftime = time[j] * 1.0e-9;
        LONS = LONS_mean * time[j];
        
        for ( k=0; k<itrigger; k++ ) {
          
          if (trigger[k] > 0) {
            ncomb = (unsigned long)combinatorial( npixels, trigger[k] );
            sm = true;
            t = trigger[k];
          } else {
            sm = false;
            t = -trigger[k];
            switch ( t ) {
            case 2:
              ncomb = 9 * rings[i] * rings[i] + 3 * rings[i];
              break;
            case 3:
              ncomb = 6 * rings[i] * rings[i];
              break;
            case 4:
              ncomb = 9 * rings[i] * rings[i] - 3 * rings[i];
              break;
            }
          }
          
          for ( l=0; l<iq0; l++ ) {
            
            rq0 = int(ceil(q0[l] + LONS));
            
            prob = pow( LONS, rq0 ) * exp( -LONS ) / fact( rq0 );
            prob *= (1 + LONS/(rq0+1) + (LONS*LONS)/((rq0+1)*(rq0+2)));
            
            rate = prob / ftime;
            atr = ncomb * pow(prob, t) / ftime;  // Hz
            
            atr /= 1000.0;  // kHz
            
            /*
              prefix = ' ';
              
              if (atr < .01) {
              atr *= 1000.;
              prefix = 'm';
              }
              if (atr > 1000.) {
              atr /= 1000.;
              prefix = 'k';
              }
              if (atr > 1000.) {
              atr /= 1000.;
              prefix = 'M';
              }
              if (atr > 1000.) {
              atr /= 1000.;
              prefix = 'G';
              }
                    
              printf("%s%d %2d %2d %2d %4d %2d %5.2f %10d %12.2g %12.2g ",
              ((sm)?"SM":"NN"), trigger[k], trigger[k], q0[l],
              rings[i], npixels, time[j], LONS, ncomb, prob, rate);
              printf("%12.2g %cHz\n", atr, prefix);
            */
          
            printf("%2d %5.1f %2d %2d %4d %2d %5.2f %10lu ",
                   trigger[k], qe[m], q0[l], rings[i], npixels, time[j],
                   LONS, ncomb);
            printf("%12.2g %12.2g %12.2g\n",
                   prob, rate, atr);
          }
        
        }

      }

    }
  
  }

  return 0;
  
}
