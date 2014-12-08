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
