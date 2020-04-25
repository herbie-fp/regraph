#!/bin/bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/regraph"

upload () {
    DIR="$(date +%s)"
    rm -rf report
    mkdir -p report
    
    cp data/upwards.csv report/upwards.csv
    cp data/rebuilding.csv report/rebuilding.csv
    cp data/umatch-counts.csv report/umatch-counts.csv
    cp data/rmatch-counts.csv report/rmatch-counts.csv
    
    cp index.css report/
    
    racket index.rkt report report/upwards.csv report/rebuilding.csv report/umatch-counts.csv report/rmatch-counts.csv report/index.html
    #rsync --perms --chmod 755 --recursive report/ "$RHOST:$RHOSTDIR/$DIR"
}

upload
