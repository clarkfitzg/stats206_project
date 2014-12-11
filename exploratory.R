library(ggplot2)

# Exploratory data analysis
load('country.Rda')
source('functions.R')

# Why is debt going down for the US? Because it's a huge negative number!
# Plot it and look at the range in order to see this.

# Exchange rate over time
qplot(Date, exchange_rate, data = country, ylab = 'Won / Dollar')
ggsave('exchange_rate.pdf', width = 6, height = 8, units = 'in')

# What's the max difference?
r = range(country$exchange_rate)
r[2] / r[1]

# Will it fit?
fit1 = lm(exchange_rate ~ ., data = trainset[, -1])
summary(fit1)
plot(fit1)

# Intentionally Remove the effects of time
timefit = lm(exchange_rate ~ Date, data=trainset)
plot(timefit)
# This is a terrible fit - exchange rate is not linear over time.
# Anyways, it would be difficult to fully remove time

country_notime = cbind(exchange_notime = trainset$exchange_rate - predict(timefit),
                       trainset[, -c(1, 2)])

fit2 = lm(exchange_notime ~ ., data = country_notime)
summary(fit2)
plot(fit2)


# Thu Dec 11 14:18:14 PST 2014

set.seed(123)

# Drop the date column
nodate = country[, names(country) != 'Date']
dim(nodate)

# Exploratory data analysis
pdf('scatterplot.pdf')
plot(nodate[, 1:5])
dev.off()

plot(nodate[, 1:5])
# Time dependent patterns are very obvious here
# We want to be seeing a cloud
# Who put these snakes in my data?!!
plot(nodate[, c(1, 6:10)])
plot(nodate[, c(1, 11:16)])
plot(nodate[, c(1, 17:21)])

# Suppose we pick out the variables where the patterns between
# exchange_rate and the other variables are least obvious?
# We can see them from the plots above.
nopattern = c('exchange_rate', 'unemployment_KOR', 'interest_rate_KOR',
              'inflation_KOR', 'interest_rate_USA', 'inflation_USA')

# What happens if we just regress on these?
fitnopattern = lm(exchange_rate ~ ., data = nodate[, nopattern])
summary(fitnopattern)
# R^2 is around 0.6, so it doesn't do all that well.

# Get the difference between each previous row
# As suggested by Taeyen
countrydiff = as.data.frame(sapply(nodate, diff))

ttsplit(countrydiff, 1 / 3, makeglobal=TRUE)

plot(trainset[, c('exchange_rate', 'interest_rate_KOR', 'interest_rate_USA')]) 

# Many of these 'diffed' variables are close to discrete

plot(trainset[, 1:5])
# This looks much more like a proper data cloud
# But I worry that exchange rate is too flat when seen as a function of the
# other variables
plot(trainset[, c(1, 6:10)])
plot(trainset[, c(1, 11:16)])
plot(trainset[, c(1, 17:21)])


mod1 = lm(exchange_rate ~ ., data=trainset)
summary(mod1)

library(leaps)

r1 = regsubsets(exchange_rate ~ ., data=trainset)
s1 = summary(r1)

showmodel = function(p, leapsummary){
    # returns the coefficient names from the model with p coefficients
    # p - 1 used because first term includes intercept
    pick = leapsummary$which[p - 1, ]
    names(pick[pick])
}

# Only picked 9- and they look like the ones that were more of a cloud from
# the EDA- the nopattern ones
nopattern
sapply(1:9, showmodel, s1)


