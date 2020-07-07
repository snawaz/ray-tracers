#!/bin/bash

set -eou pipefail

secs_to_human() {
    echo "$(( (${1} / 60) % 60 ))m-$(( ${1} % 60 ))s"
}

cabal v2-build -O2

date

s=$(date +%s)

time ./ray-tracers +RTS -N -s -l >> tmp.ppm

date

e=$(date +%s)

# suffix=$(date --date="@$(($e - $s))" +%mm%Ss)
filename="images/$e-$(secs_to_human $(($e - $s))).ppm"
mv tmp.ppm $filename

echo -e "\033[32mIMAGE CREATE: $filename\033[0m\n"
