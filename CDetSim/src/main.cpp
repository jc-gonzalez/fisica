#include <iostream>
#include <cmath>

#include "mathtools.h"
using namespace MathTools;

#include "Simulator.h"

int main(int argc, char * argv[]) 
{
    Simulator & sim = Simulator::getInstance();
    
    sim.readConfiguration("simulator.param.json");
    sim.showConfiguration();

    sim.run();

    matrix3D  m = makeOmega( d2r(45.0), d2r(30.0) );
    matrix3D  mi = makeOmega( d2r(45.0), d2r(30.0) );

    vector3D v {1.0, 2.0, -1.2};

    std::cout << v << '\n';

    vector3D vp = applyMxV(m, v);
    v = applyMxV(mi, vp);

    std::cout << vp << '\n';
    std::cout << v << '\n';
    
    return 0;
}
