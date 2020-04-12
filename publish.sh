#!/bin/bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/regraph"

upload () {
    DIR="$(date +%s)"
    mkdir -p report
    mv search-time.png total-time.png congruence-closure-time.png report/
    mv tables-upwards/averages.txt report/upwards.txt
    mv tables-rebuilding/averages.txt report/rebuilding.txt
    mv tables-upwards/benchmarks.txt report/benchmarks-upwards.txt
    mv tables-rebuilding/benchmarks.txt report/benchmarks-rebuilding.txt
    cp timing-upwards/match-counts-verification.txt report/match-counts-upwards.txt
    cp timing-rebuilding/match-counts-verification.txt report/match-counts-rebuilding.txt
    cp index.css report/
    racket index.rkt report/upwards.txt report/rebuilding.txt report/benchmarks-upwards.txt report/benchmarks-rebuilding.txt report/match-counts-upwards.txt report/match-counts-rebuilding.txt report/index.html
    rsync --perms --chmod 755 --recursive report/ "$RHOST:$RHOSTDIR/$DIR"
}

upload
