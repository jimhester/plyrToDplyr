R_SOURCE = $(wildcard R/*.r)

all: index.html

index.html: plyrToDplyr.Rmd $(R_SOURCE)
	Rscript -e "\
    library(knitr);\
    library(knitrBootstrap);\
    library(rmarkdown);\
    render('plyrToDplyr.Rmd')"
	cp plyrToDplyr.html index.html

clean:
	rm -rf index.html R/*.Rmd plyrToDplyr_{files,cache} R/*.md
