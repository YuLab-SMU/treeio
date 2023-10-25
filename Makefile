PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
BIOCVER := RELEASE_3_18

all: rd check clean

alldocs: rd readme

rd:
	Rscript -e 'library(methods); devtools::document()'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

codemetar:
	Rscript -e 'codemetar::write_codemeta()'

sticker:
	Rscript -e 'source("treeio_sticker.R")';
	rm Rplots.pdf

build:
	#cd ..;\
	# R CMD build $(PKGSRC)
	Rscript -e 'devtools::build()'

build2:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz


check: 
	# cd ..;\
	# Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz")'
	Rscript -e 'devtools::check()'

check2: rd build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

check3: rd build2
	cd ..;\
	R CMD check --ignore-vignettes $(PKGNAME)_$(PKGVERS).tar.gz

bioccheck:
	cd ..;\
	Rscript -e 'BiocCheck::BiocCheck("$(PKGNAME)_$(PKGVERS).tar.gz")'

gpcheck:
	Rscript -e 'goodpractice::gp()'

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/


gitmaintain:
	git gc --auto;\
	git prune -v;\
	git fsck --full

rmrelease:
	git branch -D $(BIOCVER)

release:
	git checkout $(BIOCVER);\
	git fetch --all
	
update:
	git fetch --all;\
	git checkout devel;\
	git merge upstream/devel;\
	git merge origin/devel

push: update
	git push upstream devel;\
	git push origin devel

biocinit:
	git remote add upstream git@git.bioconductor.org:packages/$(PKGNAME).git;\
	git fetch --all
