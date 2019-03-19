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

    return 0;
}
