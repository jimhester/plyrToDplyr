library(dplyr)

bnames <- read.csv("../data/bnames.csv", stringsAsFactors = FALSE)
head(bnames)

# Whole dataset transformations ---------------------------------------------
letter <- function(x, n = 1) {
  if (n < 0) {
    nc <- nchar(x)
    n <- nc + n + 1
  }
  tolower(substr(x, n, n))
}
vowels <- function(x) {
  nchar(gsub("[^aeiou]", "", x))
}

bnames <- mutate(bnames,
  first = letter(name, 1),
  last = letter(name, -1),
  length = nchar(name),
  vowels = vowels(name)
)

# Whole dataset summaries ----------------------------------------------------

summarise(bnames,
  max_perc = max(percent),
  min_perc = min(percent))


# Group-wise transformations  ------------------------------------------------

# Want to calculate rank of each name in each year (per sex).  This is easy if
# we have a single sex for a single year:
one <- filter(bnames, sex == "boy", year == 2008)
one$rank <- rank(-one$percent, ties.method = "first")
# or
one <- mutate(one, rank = rank(-percent, ties.method = "first"))
head(one)

# Conceptually if we want to perform this same task for every sex in every 
# year, we need to split up the data, apply the transformation to every piece
# and then join the pieces back together

# This is what group_by and mutate do
bnames <- bnames %>% group_by(sex, year) %>% mutate(rank = rank(-percent, ties.method = "first"))

# Group-wise summaries -------------------------------------------------------

# Group-wise summaries are much more interesting!

bnames %>% group_by(name) %>% summarise(tot = sum(percent))
bnames %>% group_by(length) %>% summarise(tot = sum(percent))
bnames %>% group_by(year, sex) %>% summarise(tot = sum(percent))

fl <- bnames %>% group_by(year, sex, first) %>% summarise(tot = sum(percent))
library(ggplot2)
qplot(year, tot, data = fl, geom = "line", colour = sex, facets = ~ first)
