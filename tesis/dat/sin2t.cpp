/*#include <iostream.h>*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double theta(double p);

int main(void)
{
  double lambda, phi;
  double x, y;
  double t;

  scanf("%lf %lf", &lambda, &phi);

  do {

    t = theta(phi * M_PI / 180) * M_PI / 180.;

    x = (2. * sqrt(2.) * lambda * cos(t)) / M_PI;
    y = sqrt(2.) * sin(t);

    printf("%16.8g %16.8g\n", x*180/M_PI, y*180/M_PI);
    
    scanf("%lf %lf", &lambda, &phi);
  
  } while (lambda > -720);

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
