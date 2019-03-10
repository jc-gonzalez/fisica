#include "mathtools.h"

#include <iostream>

using namespace MathTools;

#define VEC(x)  "(" << x[0] << ", " << x[1] << ", " << x[2] << ")"

int main(int argc, char * argv[])
{
    float Omega[3][3];
    float OmegaI[3][3];

    makeOmega(Omega, d2r(45.), 0.);
    makeOmega(OmegaI, d2r(-45.), 0.);

    float u[3] {1., 4., 8.};
    float v[3];
    float w[3];

    applyMxV(Omega, u, v);
    applyMxV(OmegaI, v, w);

    std::cout << VEC(u)  << "  :  " << VEC(v) << "  :  " << VEC(w) << '\n';

    return 0;
}

