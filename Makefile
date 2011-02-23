all: roxy build check

roxy: clean src/$(PKG)/DESCRIPTION src/$(PKG)/R/*.R 
	rm -f pkg/$(PKG)/man/*.Rd
	Rscript -e "library (roxygen); roxygenize (\"src/$(PKG)\", \"pkg/$(PKG)\", use.Rd2 = TRUE)" --vanilla
	rsync -av --delete src/$(PKG)/R/*.R pkg/$(PKG)/R/

src/$(PKG)/DESCRIPTION:
	touch $@

src/$(PKG)/R/*.R: 
	touch $@

clean:
	rm -f src/*/R/#*.R#
	rm -f src/*/R/*.R~
	rm -f pkg/*/man/.*.Rd
	rm -f */*/*/.Rhistory
	find -maxdepth 5 -name ".Rhistory" -delete

check: roxy
	R CMD check pkg/$(PKG) && rm -rf $(PKG).Rcheck
	Rscript --vanilla -e "library ($(PKG)); $(PKG).unittest()"

build: roxy
	R CMD build pkg/$(PKG) 

