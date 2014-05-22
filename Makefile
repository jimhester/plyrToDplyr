R_SOURCE = $(wildcard R/*.r)

all: plyrToDplyr.html

plyrToDplyr.html: plyrToDplyr.Rmd $(R_SOURCE)
	Rscript -e "\
    library('knitr');\
    library('knitrBootstrap');\
    library('rmarkdown');\
    render('plyrToDplyr.Rmd')"

clean:
	rm -rf plyrToDplyr.html R/*.Rmd plyrToDplyr_{files,cache} R/*.md
