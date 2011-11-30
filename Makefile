all: roxy build check test

roxy: clean DESCRIPTION src/R/*.R 
#	rm -f pkg/man/*.Rd
	rsync -av --delete src/R/*.R pkg/R/
	rsync -av --delete src/tests/* pkg/tests/
	Rscript --vanilla -e "library (roxygen2); roxygenize (\"pkg\")" 
	rm -rf pkg/inst

DESCRIPTION: $(shell find src -maxdepth 1 -daystart -not -ctime 0 -name "DESCRIPTION") #only if not modified today
	@echo update DESCRIPTION
	sed "s/\(^Version: .*-\)20[0-9][0-9][0-1][0-9][0-3][0-9]\(.*\)$$/\1`date +%Y%m%d`\2/" src/DESCRIPTION > .DESCRIPTION
	sed "s/\(^Date: .*\)20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]\(.*\)$$/\1`date +%F`\2/" .DESCRIPTION > src/DESCRIPTION 
	rm .DESCRIPTION

src/R/*.R: 
	touch $@

clean:
	rm -f src/R/#*.R#
	rm -f src/R/*.bak
	rm -f src/R/*.R~
	cd pkg && rm -rf *.R~	
	rm -f pkg/*/man/.*.Rd
	find -maxdepth 4 -name ".Rhistory" -delete

check: 
	R CMD check pkg --vanilla && rm -rf pkg.Rcheck 

install:
		sudo R CMD INSTALL pkg	

test: install
	Rscript --vanilla -e "library (softclassval); softclassval.unittest ()"

build: roxy
	R CMD build pkg --vanilla

devbuild: roxy
	~/r-devel/bin/R CMD build pkg --vanilla

devcheck: roxy
	~/r-devel/bin/R CMD check pkg --vanilla

devtest: roxy
	~/r-devel/bin/R CMD INSTALL pkg
	~/r-devel/bin/Rscript --vanilla -e "library (softclassval); softclassval.unittest ()"
