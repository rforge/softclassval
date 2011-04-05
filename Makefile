all: roxy build check test

roxy: clean src/DESCRIPTION src/R/*.R 
	rm -f pkg/man/*.Rd
	Rscript --vanilla -e "library (roxygen); roxygenize (\"src\", \"pkg\", use.Rd2 = TRUE)" 
	rsync -av --delete src/R/*.R pkg/R/
	rm -rf pkg/inst

src/DESCRIPTION:
	touch $@

src/R/*.R: 
	touch $@

clean:
	rm -f src/R/#*.R#
	rm -f src/R/*.R~
	cd pkg && rm -rf *.R~	
	rm -f pkg/*/man/.*.Rd
	find -maxdepth 4 -name ".Rhistory" -delete

check: 
	R CMD check pkg --vanilla && rm -rf pkg.Rcheck 

test: 
	sudo R CMD INSTALL pkg	
	Rscript --vanilla -e "library (softclassval); softclassval.unittest ()"

build: roxy
	R CMD build pkg --vanilla

