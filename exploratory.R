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

# Get the difference between each previous row
countrydiff = as.data.frame(sapply(country, diff))

# Drop the date column
countrydiff = countrydiff[, -1]

ttsplit(countrydiff, 1 / 3, makeglobal=TRUE)

mod1 = lm(exchange_rate ~ ., data=trainset)
summary(mod1)

library(leaps)

r1 = regsubsets(exchange_rate ~ ., data=trainset)
s1 = summary(r1)
