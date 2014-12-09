library(ggplot2)

# Exploratory data analysis
load('country.Rda')

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

country_notime = cbind(exchange_notime = trainset$exchange_rate - predict(timefit),
                       trainset[, -c(1, 2)])
fit2 = lm(exchange_notime ~ ., data = country_notime)
summary(fit2)
plot(fit2)
