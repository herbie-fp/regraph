.PHONY: nightly scrape
SCRAPELOG=http://warfa.cs.washington.edu/nightlies/2020-01-20-herbie-master.log

nightly:
	cd infra && bash scrape.sh ${SCRAPELOG}
	cd infra && bash time.sh
	cd infra && bash publish.sh ${SCRAPELOG}
