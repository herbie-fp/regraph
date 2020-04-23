#!/bin/bash
set -o errexit -o pipefail

HERBIERUN="http://warfa.cs.washington.edu/nightlies/2020-04-15-herbie-master.log"

echo "scraping expressions from run at $HERBIERUN"

rm -rf exprs
mkdir -p exprs

curl -s "$HERBIERUN" \
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


