#!/bin/bash

x=$1
y=$2
z=$3

g=$4

cat <<EOF
set pm3d at b
set view map
set dgrid $g
splot "<awk -v x=$x -v y=$y -v z=$z '(\$1==1){print \$x,\$y,\$z;}' out" u 1:2:3 w pm3d
EOF
