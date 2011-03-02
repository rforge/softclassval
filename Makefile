all: roxy build check test

roxy: clean src/$(PKG)/DESCRIPTION src/$(PKG)/R/*.R 
	rm -f pkg/$(PKG)/man/*.Rd
	Rscript --vanilla -e "library (roxygen); roxygenize (\"src/$(PKG)\", \"pkg/$(PKG)\", use.Rd2 = TRUE)" 
	rsync -av --delete src/$(PKG)/R/*.R pkg/$(PKG)/R/
	rm -rf pkg/$(PKG)/$(PKG)
	rm -rf pkg/$(PKG)/inst

src/$(PKG)/DESCRIPTION:
	touch $@

src/$(PKG)/R/*.R: 
	touch $@

clean:
	rm -f src/*/R/#*.R#
	rm -f src/*/R/*.R~
	cd pkg && rm -rf *.R~	
	rm -f pkg/*/man/.*.Rd
	rm -f */*/*/.Rhistory
	find -maxdepth 5 -name ".Rhistory" -delete

check: 
	R CMD check pkg/$(PKG) --vanilla && rm -rf $(PKG).Rcheck 

test: 
	Rscript --vanilla -e "library ($(PKG)); $(PKG).unittest()"

build: roxy
	R CMD build pkg/$(PKG) --vanilla

