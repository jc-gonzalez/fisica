#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

#define RandomNumber  drand48()

float
get_new_ct_pointing(float theta, float phi, float range,
                    float *newtheta, float *newphi,
                    float *iit, float *iip);

int
main(int argc, char **argv)
{
  float theta, phi;
  float newtheta, newphi;
  float it, ip;
  float t;
  
  theta = atof( argv[1] ) * M_PI / 180.0;
  phi   = atof( argv[2] ) * M_PI / 180.0;

  for (int i=0; i<100000; i++) {

    t = get_new_ct_pointing(theta, phi, 45. * M_PI / 180.0,
                            &newtheta, &newphi, &it, &ip);
    cout << it*57.3 << ' '
         << ip*57.3 << ' '
         << newtheta*57.3 << ' '
         << newphi*57.3 << ' ';
    cout << sin( newtheta ) * cos( newphi ) << ' '
         << sin( newtheta ) * sin( newphi ) << ' '
         << cos( newtheta ) << endl;
    
  }

  return 0;
        
}

float
get_new_ct_pointing(float theta, float phi, float range,
                    float *newtheta, float *newphi,
                    float *iit, float *iip)
{
  float distance;
  float it, ip;
  float sin_theta, cos_theta;
  float sin_newtheta, cos_newtheta;
  float sin_iphi, cos_iphi;
  float iphi;
  float ipsign;

  it = acos(cos(range) + RandomNumber * (1 - cos(range)));
  //it = RandomNumber * range;
  ip = RandomNumber * 2.0 * M_PI;

  sin_theta = sin(theta);
  cos_theta = cos(theta);

  cos_newtheta = cos_theta*cos(it) + sin_theta*sin(it)*cos(ip);
  *newtheta = acos( cos_newtheta );
  sin_newtheta = sin( *newtheta );

  sin_iphi = sin(it)*sin(ip) / sin_newtheta;
  cos_iphi = (( cos(it) - cos_newtheta * cos_theta ) /
              ( sin_newtheta * sin_theta ));

  iphi = atan2( sin_iphi, cos_iphi );

  *newphi = phi + iphi;

  *iit = it;
  *iip = ip;

  return( it );
}
