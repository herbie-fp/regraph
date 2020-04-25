#!/bin/bash
set -o errexit -o pipefail

echo "scraping expressions from run at $1"

rm -rf exprs
mkdir -p exprs

curl -s "$1" \
    | sed -n -e '/version:/,/version:/ p' \
    | grep "rsync --recursive reports/" \
    | cut -d\   -f4,7 \
    | while read -r argsuite argpath; do
    suite=`echo $argsuite | cut -d/ -f2`
    reportid=`echo $argpath | cut -d/ -f6`
    script="find \"/var/www/herbie/reports/$reportid\" -name debug.txt.gz -exec zcat {} ';'"
    
    
    echo "scraping benchmark suite $suite"
    ssh -n oflatt@uwplse.org "$script" \
	| sed -n -e '/Simplifying using/,/iteration/ p' \
  	| sed '/Simplifying using/ c (' \
	| sed '/iteration/ c )' \
	      > "exprs/$suite.exprs"
	
	
    echo "scraped to exprs/$suite.exprs"
    
    done


