all: roxy build check

roxy: clean src/arrayhelpers/DESCRIPTION src/arrayhelpers/R/*.R 
	rm -f pkg/arrayhelpers/man/*.Rd
	Rscript -e "library (roxygen); roxygenize ('src/arrayhelpers', 'pkg/arrayhelpers', use.Rd2 = TRUE)" 
	rsync -av --delete src/arrayhelpers/R/*.R pkg/arrayhelpers/R/

src/arrayhelpers/DESCRIPTION:
	touch $@

src/arrayhelpers/R/*.R: 
	touch $@

clean:
	rm -f src/*/R/*.R~
	rm -f pkg/*/man/.*.Rd
	rm -f */*/*/.Rhistory
	find -maxdepth 5 -name ".Rhistory" -delete

check: roxy
	R CMD check pkg/arrayhelpers && rm -rf arrayhelpers.Rcheck
