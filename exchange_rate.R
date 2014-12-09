# Exploratory data analysis

load('country.Rda')

pdf('exchange_rate.pdf')

with(country, plot(Date, exchange_rate, ylab = 'Won / Dollar'))

dev.off()
