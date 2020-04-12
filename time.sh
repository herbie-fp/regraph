#!/bin/bash
set -e -x

export PATH="$PATH:/opt/racket-7.5/bin/"

if [ ! -d herbie ] ; then
    git clone https://github.com/uwplse/herbie
fi

mkdir -p "data"

rm -rf "data/timing-upwards"
rm -rf "data/tables-upwards"
rm -rf "data/timing-rebuilding"
rm -rf "data/tables-rebuilding"
rm -rf "data/limits"

mkdir -p "data/limits"
mkdir -p "data/timing-upwards"
mkdir -p "data/timing-rebuilding"

raco make -v ./main.rkt

racket time-regraph.rkt data/timing-upwards data/timing-rebuilding data/limits exprs/*.txt

racket process-data.rkt data/timing-upwards data/timing-rebuilding data/tables-upwards data/tables-rebuilding data/all
