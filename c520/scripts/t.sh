#!/bin/sh
#set -x
cat /dev/null > times
for e in 10 20 40 60 100 500 1000; do
    cat time$e | \
    awk '{printf "%s %s ",$1,$2}' |\
    awk '{print '$e',$2,$4,$6,$7/10,$8/10,($7+$8)/10}' >> times
done

