.PHONY: nightly scrape
SCRAPELOG=http://warfa.cs.washington.edu/nightlies/2020-03-18-herbie-master.log

nightly:
	cd infra && bash time.sh
	cd infra && bash publish.sh ${SCRAPELOG}


scrape:
	cd infra && bash scrape.sh ${SCRAPELOG}
