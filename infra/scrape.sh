#!/bin/bash

echo "scraping expressions from run at $1"

rsynclines=$(curl -s "$1" \
		 | grep "rsync --recursive r")

urlids=(`echo "$rsynclines" | grep -oEi "herbie/reports/.*" | xargs -n1 basename`)

mkdir -p exprs

NL=$'\n'

for ((x=0; x<(${#urlids[@]} / 2); x++)); do
    urlid="${urlids[x]}"
    benchname=$(ssh oflatt@uwplse.org "dir /var/www/herbie/reports/$urlid")
    echo "scraping benchmark suite $benchname"
    alldebuglogscommand="testnames=\$(ls /var/www/herbie/reports/$urlid/$benchname
			       		  | grep -Ei \"[0-9]+-\") &&
			 for testname in \$testnames; do
			     zcat /var/www/herbie/reports/$urlid/$benchname/\$testname/debug.txt.gz
			     | sed -n -e '/Simplifying using/,/iteration/ p'
			     | sed '/iteration/ d'
			     | sed '/Simplifying using/ c \"NEW BATCH\"';
 			 done"
    ssh oflatt@uwplse.org $alldebuglogscommand >> "exprs/$benchname-exprs.txt"
done
