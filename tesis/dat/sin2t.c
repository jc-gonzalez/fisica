/*#include <iostream.h>*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define RENORM(x) (((x)>180.)?(x-360.):(x))

double theta(double p);

int main(void)
{
  float flambda, fphi;
  double lambda, phi;
  double x, y;
  double t;

  scanf("%f %f\n", &flambda, &fphi);

  /* lambda       -180:180 */
  /* phi           -90:90  */
  
  lambda = RENORM( (double)flambda );
  phi = (double)fphi;

  do {

    t = theta(phi * M_PI / 180);
    
    x = lambda * cos(t);
    y = 90. * sin(t);
    
    printf("%16.8g %16.8g\n", x, y);
    
    scanf("%f %f\n", &flambda, &fphi);
    
    lambda = RENORM( (double)flambda );
    phi = (double)fphi;
    
  } while (lambda > -1000.);
  
  return 0;
}

double theta(double p)
{
  double t;
  double ot;
  double diff;
  double epsilon = 1.0e-4;

  double pi_2;
  double a, b;
  
  int i, j;
  int c;

  pi_2 = M_PI / 2.0;
  
  a = pi_2 * 3. / 4.;
  b = pi_2 * 5. / 4.;
  
  if (fabs(p) < a) 
    t = (M_PI * sin(p)) / 4.0;
  else if (fabs(p) > b) 
    t = (M_PI * sin(p)) / 4.0 - M_PI;
  else 
    t = p;
    
  for (i=0; i<10000; i++) {
      
    ot = t;
    t = 0.5 * (M_PI * sin(p) - sin(2 * t));
    
    diff = t - ot;
    if (fabs(diff) < epsilon)
      break;
    
  }
    
  return t;
}
