#include <Histogram.hxx>

#define real float

void
main(int argc, char **argv)
{
  float v;
  real x=1.;
  real xmin, xmax;
  int nbins;

  nbins = atoi(argv[1]);
  xmin = atoi(argv[2]);
  xmax = atoi(argv[3]);
  
  Histogram<real> h("prueba", nbins, xmin, xmax);

  while (x>-10000.) {
    scanf("%f", &v);
    x = (real)v;
    if (x>-10000.) 
      h.fill(x,1.);
  }

  h.calcerr();
  h.print();
  h.dump();
  return;
}
 
