/*#include <iostream.h>*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double theta(double p);

int main(void)
{
  float flambda, fphi;
  double lambda, phi;
  double x, y;
  double t;
  int n;
  
/*   scanf("%f %f\n", &flambda, &fphi); */

/*   lambda = (double)flambda; */
/*   phi = (double)fphi; */

/*   do { */

  n = 0;
  
  for (lambda=-180.; lambda<181.; lambda+=45.) {
    
    for (phi=-90.; phi<91.; phi+=5.) {

      t = theta(phi * M_PI / 180);
      
/*       x = 2 * sqrt(2.) * lambda * cos(t) / M_PI; */
/*       y = sqrt(2.) * sin(t); */

      x = lambda * cos(t);
      y = 90. * sin(t);

      printf("%d %16.8g %16.8g %16.8g %16.8g\n",
             n, lambda, phi, x, y);

    }
    
    n++;
      
  }
    
/*     scanf("%f %f\n", &flambda, &fphi); */

/*     lambda = (double)flambda; */
/*     phi = (double)fphi; */
  
/*   } while (lambda > -720.); */

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
