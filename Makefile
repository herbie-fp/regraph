.PHONY: nightly scrape
SCRAPELOG=http://warfa.cs.washington.edu/nightlies/2020-03-18-herbie-master.log

nightly:
	cd infra && bash time.sh
	cd infra && bash publish.sh
	mv infra/report ./report


scrape:
	cd infra && bash scrape.sh ${SCRAPELOG}
