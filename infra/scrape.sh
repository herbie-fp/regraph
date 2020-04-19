#!/bin/bash

rsynclines=$(curl -s http://warfa.cs.washington.edu/nightlies/2020-04-15-herbie-master.log \
		 | grep "rsync --recursive r")

urlids=(`echo "$rsynclines" | grep -oEi "herbie/reports/.*" | xargs -n1 basename`)

benchnames=(`echo "$rsynclines" \
		| grep -oEi "recursive\ reports/[a-z]+" \
		| cut -d "/" -f 2`)

unique=($(echo "${benchnames[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))


for ((i=0;i<${#unique[@]};++i)); do
    urlid="${urlids[i]}"
    benchname="${benchnames[i]}"
    echo $benchname
    tests=$(curl -s -L "http://herbie.uwplse.org/reports/$urlid/$benchname")
done

    #





#    | tee >(grep -oEi "recursive\ reports/[a-z]+" | cut -d "/" -f 2) \#
#	  >(grep -oEi "herbie/reports/*" | cut -d "/" -f 3) \
#    | paste

  #  | grep -oEi "find\ reports/[a-z]+" \
  #  | cut -d "/" -f 2 \
  #  | sort | uniq
