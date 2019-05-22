#!/bin/bash -x
#./corsika76400Linux_QGSJET_gheisha < $1
echo "$*"
if [ -z "$1" ]; then 
    /bin/bash
else
    cd /c764/run
    ./c764 < $1
fi


