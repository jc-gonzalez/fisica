#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(void)
{
  double x, y;
  double t;
  int i;

  i = 0;
  
  for (x = 0.; x < 181.; x += 45.) {
    
    for (y = -90.; y < 91.; y += 5.) {
      printf("%d %f %f\n", i, x, y);
    }
    
    for (y = -90.; y < 91.; y += 5.) {
      printf("%d %f %f\n", i, -x, -y);
    }
    
    i++;
  }
    
  return 0;
}
