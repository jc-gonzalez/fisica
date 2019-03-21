#include "mathtools.h"
#include "quaternions.h"

#include <iostream>

using namespace MathTools;

#define VEC(x)  "(" << x[0] << ", " << x[1] << ", " << x[2] << ")"

int main(int argc, char * argv[])
{
    float Omega[3][3];
    float OmegaI[3][3];

    makeOmega<float>(Omega, d2r<float>(45.), 0.);
    makeOmega<float>(OmegaI, d2r<float>(-45.), 0.);

    float u[3] {1., 4., 8.};
    float v[3];
    float w[3];

    applyMxV<float>(Omega, u, v);
    applyMxV<float>(OmegaI, v, w);

    std::cout << VEC(u)  << "  :  " << VEC(v) << "  :  " << VEC(w) << '\n';

    quaternion q0(1, 2, 3, 4);
    quaternion q1(2, 3, 4, 5);
    quaternion q2(3, 4, 5, 6);
    double r = 7;

    std::cout << "q0:      " << q0 << std::endl;
    std::cout << "q1:      " << q1 << std::endl;
    std::cout << "q2:      " << q2 << std::endl;
    std::cout << "r:       " << r << std::endl;
    std::cout << std::endl;
    std::cout << "-q0:     " << -q0 << std::endl;
    std::cout << "~q0:     " << ~q0 << std::endl;
    std::cout << std::endl;
    std::cout << "r * q0:  " << r*q0 << std::endl;
    std::cout << "r + q0:  " << r+q0 << std::endl;
    std::cout << "q0 / r:  " << q0/r << std::endl;
    std::cout << "q0 - r:  " << q0-r << std::endl;
    std::cout << std::endl;
    std::cout << "q0 + q1: " << q0+q1 << std::endl;
    std::cout << "q0 - q1: " << q0-q1 << std::endl;
    std::cout << "q0 * q1: " << q0*q1 << std::endl;
    std::cout << "q0 / q1: " << q0/q1 << std::endl;
    std::cout << std::endl;
    std::cout << "q0 * ~q0:     " << q0*~q0 << std::endl;
    std::cout << "q0 + q1*q2:   " << q0+q1*q2 << std::endl;
    std::cout << "(q0 + q1)*q2: " << (q0+q1)*q2 << std::endl;
    std::cout << "q0*q1*q2:     " << q0*q1*q2 << std::endl;
    std::cout << "(q0*q1)*q2:   " << (q0*q1)*q2 << std::endl;
    std::cout << "q0*(q1*q2):   " << q0*(q1*q2) << std::endl;
    std::cout << std::endl;
    std::cout << "||q0||:  " << sqrt(q0.norm2()) << std::endl;
    std::cout << std::endl;
    std::cout << "q0*q1 - q1*q0: " << (q0*q1 - q1*q0) << std::endl;

    quaternion ww(1., 1., 1.);
    std::cout << std::endl << ww << " reflected is ";
    ww.reflect(quaternion(0., 0., 0., -1.));
    std::cout << ww << std::endl;

    return 0;
}

