library(ggplot2)
library(dplyr)
options(na.action = "na.exclude")

# Helper functions ----------------------------------------------------------
deseas <- function(var, month) {
  resid(lm(var ~ factor(month))) + mean(var, na.rm = TRUE)
}

# Explore multiple cities ----------------------------------------------------
tx <- read.csv("../data/tx-house-sales.csv")

# We know from our exploration of Houston data that many of the series
# have strong seasonal components.  It's a good idea to check that's true for 
# all cities.  We'll start with sales.
qplot(date, sales, data = tx, geom = "line", group = city)

# Hmmmm.  Problem!  There's a large variation in the number of sales between
# cities.  The seasonal pattern does look pretty constant though.

# First possible solution, just remove the seasonal effect as we did for a 
# single city, but applied to multiple cities (using model as a tool)  
tx_city <- tx %>% group_by(city)

tx = tx_city %>% mutate(sales_ds = deseas(sales, month))

qplot(date, sales_ds, data = tx, geom = "line", group = city)

# It works, but we don't gain much insight into what's going on.
# Let's fit the models, and actually look at them this time
models = tx %>% group_by(city) %>% do(model=lm(sales ~ factor(month), data=.))

models[[1, 'model']]

coef(models[[1, 'model']])

# To extract the coefficients, we want to go from a list to data frame
# Notice how plyr remembers the city names that we originally broke the
# models up by.
(models %>% group_by(city) %>% do(data.frame(t(coef(.[[1, 'model']])), check.names=FALSE)))[1:5, 1:3]

# Two problems with the model:
#   * Coefficients aren't comparable, because of varying sizes
#     Solution: log-transform to convert to ratios
#   * Coefficients not in useful form for plotting
#     Solution: create data frame ourselves
qplot(date, log10(sales), data = tx, geom = "line", group = city)

models2 <- tx_city %>% do(model=lm(log10(sales) ~ factor(month), data = .))

coef2 <- models2 %>% group_by(city) %>% do((function(row) {
  mod = row[['model']][[1]]
  data.frame(
    month = 1:12,
    effect = c(0, coef(mod)[-1]),
    intercept = coef(mod)[1])
})(.))


# Pretty consistent pattern, although there are few outliers
qplot(month, effect, data = coef2, group = city, geom = "line")
# More interpretable if we back-transform - can now interpret as ratios
qplot(month, 10 ^ effect, data = coef2, group = city, geom = "line")
# What are the outliers?
qplot(month, 10 ^ effect, data = coef2, geom = "line") + facet_wrap(~ city)
# They are small cities. Hmmmmm

# Have a look at the distributions
qplot(effect, data = coef2, binwidth = 0.05) + facet_wrap(~ month)

# Single model ----------------------------------------------------

mod <- lm(log10(sales) ~ city + factor(month), data = tx)

tx$sales2 <- 10 ^ resid(mod)
qplot(date, sales2, data = tx, geom = "line", group = city)
# Now we're starting to get somewhere!  Can see general pattern, although
# there are a few outliers.  Look at cities individually to identify:
last_plot() + facet_wrap(~ city)
# Some problem cities:
#   * Bryan-College station: has different seasonal pattern (Texas A&M?)
#   * Similarly with San Marcos (a lot of missing data)
#   * Palestine: massive increase beginning 2007

# Can resolve seasonal problems by fitting separate seasonal pattern to each
# city (Challenge: how is this different to the indivudal models we fit 
# before?)  But probably better to use more sophisticated model (e.g. mixed 
# effects) model.
mod2 <- lm(log10(sales) ~ city:factor(month), data = tx)
tx$sales3 <- 10 ^ resid(mod2)
qplot(date, sales3, data = tx, geom = "line") + facet_wrap(~ city)

# Further exploration
qplot(date, sales2, data = tx, geom = "line", group = city, alpha = I(0.2))
last_plot() + geom_smooth(aes(group = 1))

# Could smooth individual cities - again just using model as a tool
library(mgcv)
smooth <- function(var, date) {
  predict(gam(var ~ s(date)))
}
tx <- tx %>% group_by(city) %>% mutate(sales2_sm = as.vector(smooth(sales2, date)))

qplot(date, sales2_sm, data = tx, geom = "line", group = city)

# Another approach -----------------------------------------------------------

# Essence of most cities is seasonal term plus long term smooth trend.  We
# could fit this model to each city, and then look for model which don't
# fit well.

library(splines)
models3 <- tx %>% group_by(city) %>% do(model=lm(log10(sales) ~ factor(month) + ns(date, 3), data = .))

# Extract rsquared from each model
rsq <- function(mod) c(rsq = summary(mod[[1]])$r.squared)
quality <- models3 %>% group_by(city) %>% summarise(rsq=rsq(model))
qplot(rsq, city, data = quality)

qplot(rsq, reorder(city, rsq), data = quality)

quality$poor <- quality$rsq < 0.7
tx2 <- inner_join(tx, quality, by = "city")

# The cities don't look particularly differnet 
qplot(date, log10(sales), data = tx2, geom = "line", colour = poor) + 
  facet_wrap(~ city) + opts(legend.position = "none")

# But we should probably look at the residuals & predictions
mfit <- models3 %>% do((function(mod) {
  data.frame(resid = resid(mod$model), pred = predict(mod$model))
})(.))

tx2 <- cbind(tx2, mfit)
qplot(date, pred, data = tx2, geom = "line", colour = poor) + 
  facet_wrap(~ city) + opts(legend.position = "none")
qplot(date, resid, data = tx2, geom = "line", colour = poor) + 
  facet_wrap(~ city) + opts(legend.position = "none")
