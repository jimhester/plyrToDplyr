library(dplyr)
library(ggplot2)

data(package='plyr', 'baseball')

# First need to create new variables that capture the number of years
# a player has played, and for each year, how far through their career
# they are.

# This is straightforward using the techniques we just learned
b2 <- tbl_df(baseball) %>%
  group_by(id) %>%
  mutate(cyear = year - min(year) + 1,
         career = (cyear - 1) / max(cyear))
b2

# Now what sort of model should we use?
bruth <- subset(b2, id == "ruthba01")
qplot(career, g, data = bruth, geom = "line")
qplot(career, g, data = b2, geom="boxplot", group = plyr::round_any(career, 0.05))

# Could we model that as two straight lines?
bruth$p <- (bruth$career - 0.5) * 100
mod <- lm(g ~ p + p:I(p > 0), data = bruth)
bruth$ghat <- predict(mod)
qplot(career, g, data = bruth, geom = "line") + 
  geom_line(aes(y = ghat), colour = "red")

# It doesn't look great, but it's a start
# Let's fit that model to every player
b2$p <- (b2$career - 0.5) * 100

models <- b2 %>% do(model=lm(data=., formula = g ~ p + p:I(p > 0)))
# Or a bit more explicitly
models <- b2 %>% do(model=(function(df) lm(data=df, g ~ p + p:I(p > 0)))(.))

nrow(models)
onem <- models[[1, 'model']]
onem
summary(onem)

coefs <- as.list(coef(onem))
names(coefs) <- c("mid", "inc", "dec")
rsq <- summary(onem)$r.squared

get_coefs <- function(model) {
  coefs <- as.list(coef(model))
  names(coefs) <- c("mid", "inc", "dec")
  data.frame(coefs, rsq = summary(model)$r.squared)
}

coefs <- models %>% do(get_coefs(.$model))

qplot(rsq, data = coefs, geom = "histogram", binwidth = 0.02)
qplot(mid, data = coefs, geom = "histogram", binwidth = 5)
qplot(inc, data = coefs, geom = "histogram", binwidth = 0.2)
qplot(dec, data = coefs, geom = "histogram", binwidth = 0.2)

qplot(rsq, inc, data=coefs)
qplot(dec, inc, data=coefs, colour=rsq)
