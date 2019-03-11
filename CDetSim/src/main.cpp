#include <iostream>
#include <cmath>

#include "mathtools.h"

using namespace MathTools;

int main(int argc, char * argv[]) 
{
    double m = M_PI;
    double m2 = sqr<double>(m);

    std::cout << m << " squared is " << m2 << '\n';
    return 0;
}




