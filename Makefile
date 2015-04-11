all: roxy build check test

roxy: clean DESCRIPTION pkg/R/*.R 
#	rsync -av --delete --exclude=man src/ pkg/
	Rscript --vanilla -e "library (roxygen2); roxygenize ('pkg')" 

DESCRIPTION: $(shell find pkg -maxdepth 1 -daystart -not -ctime 0 -name "DESCRIPTION") #only if not modified today
	@echo update DESCRIPTION
	sed "s/\(^Version: .*-\)20[0-9][0-9][0-1][0-9][0-3][0-9]\(.*\)$$/\1`date +%Y%m%d`\2/" pkg/DESCRIPTION > .DESCRIPTION
	sed "s/\(^Date: .*\)20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]\(.*\)$$/\1`date +%F`\2/" .DESCRIPTION > pkg/DESCRIPTION 
	rm .DESCRIPTION

pkg/R/*.R: 
	touch $@

clean:
	rm -f softclassval*.tar.gz
	rm -f pkg/R/#*.R#
	rm -f pkg/R/*.bak
	rm -f pkg/R/*.R~
	cd pkg && rm -rf *.R~	
	rm -f pkg/*/man/.*.Rd
	find -maxdepth 4 -name ".Rhistory" -delete
	rm -rf *.Rcheck

check: 
	R CMD check softclassval_*.tar.gz

install:
		R CMD INSTALL pkg	

test: install
	Rscript --vanilla -e "library (softclassval); softclassval.unittest ()"

build: roxy
	R CMD build pkg --vanilla

devbuild: roxy
	~/r-devel/bin/R CMD build pkg --vanilla

devcheck: devbuild
	~/r-devel/bin/R CMD check softclassval_*.tar.gz

devtest: devbuild
	~/r-devel/bin/R CMD INSTALL softclassval_*.tar.gz
	~/r-devel/bin/Rscript --vanilla -e "library (softclassval); softclassval.unittest ()"
