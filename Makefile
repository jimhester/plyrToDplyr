R_SOURCE = $(wildcard R/*.r)

all: index.html

plyrToDplyr.html: plyrToDplyr.Rmd $(R_SOURCE)
	Rscript -e "\
    library(knitr);\
    library(knitrBootstrap);\
    library(rmarkdown);\
    render('plyrToDplyr.Rmd')"

publish: plyrToDplyr.html
	git checkout gh-pages
	cp plyrToDplyr.html index.html

clean:
	rm -rf *.html R/*.Rmd plyrToDplyr_{files,cache} R/*.md
