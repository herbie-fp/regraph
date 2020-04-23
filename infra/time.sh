#!/bin/bash
set -e -x

export PATH="$PATH:/opt/racket-7.5/bin/"

if [ ! -d herbie ] ; then
    git clone https://github.com/uwplse/herbie
fi

rm -rf "data"
mkdir -p "data"

raco make -v ../main.rkt

# upwards.csv contains suite, total time, merge time, rebuild time, search time

racket time-regraph.rkt data/upwards.csv data/rebuilding.csv data/umatch-counts.csv data/rmatch-counts.csv exprs/*.exprs
