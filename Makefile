.PHONY: nightly scrape
SCRAPELOG=http://warfa.cs.washington.edu/nightlies/2020-02-26-herbie-master.log
# Log that produces outliers- http://warfa.cs.washington.edu/nightlies/2020-04-15-herbie-master.log

nightly:
	cd infra && bash scrape.sh ${SCRAPELOG}
	cd infra && bash time.sh
	cd infra && bash publish.sh ${SCRAPELOG}
