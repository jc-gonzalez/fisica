#include <iostream>
#include <cmath>

#include "mathtools.h"

#include "Simulator.h"

using namespace MathTools;

int main(int argc, char * argv[]) 
{
    Simulator & sim = Simulator::getInstance();
    
    sim.readConfiguration("simulator.param.json");
    return 0;
}
